/*
 * Copyright (C) 2008 Tony Wasserka
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */


#include "d3dx9_private.h"

WINE_DEFAULT_DEBUG_CHANNEL(d3dx);

struct d3dx_glyph
{
    UINT id;
    RECT blackbox;
    POINT cellinc;
    IDirect3DTexture9 *texture;
};

struct d3dx_font
{
    ID3DXFont ID3DXFont_iface;
    LONG ref;

    IDirect3DDevice9 *device;
    D3DXFONT_DESCW desc;

    HDC hdc;
    HFONT hfont;

    struct d3dx_glyph *glyphs;
    UINT allocated_glyphs, glyph_count;

    IDirect3DTexture9 **textures;
    UINT texture_count, texture_pos;

    UINT texture_size, glyph_size, glyphs_per_texture;
};

static inline struct d3dx_font *impl_from_ID3DXFont(ID3DXFont *iface)
{
    return CONTAINING_RECORD(iface, struct d3dx_font, ID3DXFont_iface);
}

static HRESULT WINAPI ID3DXFontImpl_QueryInterface(ID3DXFont *iface, REFIID riid, void **out)
{
    TRACE("iface %p, riid %s, out %p.\n", iface, debugstr_guid(riid), out);

    if (IsEqualGUID(riid, &IID_ID3DXFont)
            || IsEqualGUID(riid, &IID_IUnknown))
    {
        IUnknown_AddRef(iface);
        *out = iface;
        return S_OK;
    }

    WARN("%s not implemented, returning E_NOINTERFACE.\n", debugstr_guid(riid));

    *out = NULL;
    return E_NOINTERFACE;
}

static ULONG WINAPI ID3DXFontImpl_AddRef(ID3DXFont *iface)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    ULONG ref = InterlockedIncrement(&font->ref);
    TRACE("%p increasing refcount to %u\n", iface, ref);
    return ref;
}

static ULONG WINAPI ID3DXFontImpl_Release(ID3DXFont *iface)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    ULONG ref = InterlockedDecrement(&font->ref);

    TRACE("%p decreasing refcount to %u\n", iface, ref);

    if (!ref)
    {
        UINT i;
        for(i = 0; i < font->texture_count; i++)
            IDirect3DTexture9_Release(font->textures[i]);

        if (font->textures)
            heap_free(font->textures);

        heap_free(font->glyphs);

        DeleteObject(font->hfont);
        DeleteDC(font->hdc);
        IDirect3DDevice9_Release(font->device);
        heap_free(font);
    }
    return ref;
}

static HRESULT WINAPI ID3DXFontImpl_GetDevice(ID3DXFont *iface, IDirect3DDevice9 **device)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);

    TRACE("iface %p, device %p\n", iface, device);

    if( !device ) return D3DERR_INVALIDCALL;
    *device = font->device;
    IDirect3DDevice9_AddRef(font->device);

    return D3D_OK;
}

static HRESULT WINAPI ID3DXFontImpl_GetDescA(ID3DXFont *iface, D3DXFONT_DESCA *desc)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);

    TRACE("iface %p, desc %p\n", iface, desc);

    if( !desc ) return D3DERR_INVALIDCALL;
    memcpy(desc, &font->desc, FIELD_OFFSET(D3DXFONT_DESCA, FaceName));
    WideCharToMultiByte(CP_ACP, 0, font->desc.FaceName, -1, desc->FaceName, ARRAY_SIZE(desc->FaceName), NULL, NULL);

    return D3D_OK;
}

static HRESULT WINAPI ID3DXFontImpl_GetDescW(ID3DXFont *iface, D3DXFONT_DESCW *desc)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);

    TRACE("iface %p, desc %p\n", iface, desc);

    if( !desc ) return D3DERR_INVALIDCALL;
    *desc = font->desc;

    return D3D_OK;
}

static BOOL WINAPI ID3DXFontImpl_GetTextMetricsA(ID3DXFont *iface, TEXTMETRICA *metrics)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    TRACE("iface %p, metrics %p\n", iface, metrics);
    return GetTextMetricsA(font->hdc, metrics);
}

static BOOL WINAPI ID3DXFontImpl_GetTextMetricsW(ID3DXFont *iface, TEXTMETRICW *metrics)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    TRACE("iface %p, metrics %p\n", iface, metrics);
    return GetTextMetricsW(font->hdc, metrics);
}

static HDC WINAPI ID3DXFontImpl_GetDC(ID3DXFont *iface)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    TRACE("iface %p\n", iface);
    return font->hdc;
}

static HRESULT WINAPI ID3DXFontImpl_GetGlyphData(ID3DXFont *iface, UINT glyph,
        IDirect3DTexture9 **texture, RECT *blackbox, POINT *cellinc)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    HRESULT hr;
    int i;

    TRACE("iface %p, glyph %#x, texture %p, blackbox %p, cellinc %p\n",
          iface, glyph, texture, blackbox, cellinc);

    hr = ID3DXFont_PreloadGlyphs(iface, glyph, glyph);
    if (FAILED(hr))
        return hr;

    for (i = 0; i < font->glyph_count; i++)
        if (font->glyphs[i].id == glyph)
        {
            if (cellinc)
                *cellinc = font->glyphs[i].cellinc;
            if (blackbox)
                *blackbox = font->glyphs[i].blackbox;
            if (texture)
                *texture = font->glyphs[i].texture;
            if (texture && *texture)
                IDirect3DTexture9_AddRef(font->glyphs[i].texture);
            return D3D_OK;
        }

    return D3DXERR_INVALIDDATA;
}

static HRESULT WINAPI ID3DXFontImpl_PreloadCharacters(ID3DXFont *iface, UINT first, UINT last)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    UINT i, count;
    WCHAR *chars;
    WORD *indices;

    TRACE("iface %p, first %u, last %u\n", iface, first, last);

    if (last < first) return D3D_OK;

    count = last - first + 1;
    indices = heap_alloc(count * sizeof(WORD));
    if (!indices)
        return E_OUTOFMEMORY;

    chars = heap_alloc(count * sizeof(WCHAR));
    if (!chars)
    {
        heap_free(indices);
        return E_OUTOFMEMORY;
    }

    for (i = 0; i < count; i++)
        chars[i] = (WCHAR)(first + i);

    GetGlyphIndicesW(font->hdc, chars, count, indices, 0);

    for (i = 0; i < count; i++)
        ID3DXFont_PreloadGlyphs(iface, indices[i], indices[i]);

    heap_free(chars);
    heap_free(indices);

    return D3D_OK;
}

static UINT morton_decode(UINT x)
{
    x &= 0x55555555;
    x = (x ^ (x >> 1)) & 0x33333333;
    x = (x ^ (x >> 2)) & 0x0f0f0f0f;
    x = (x ^ (x >> 4)) & 0x00ff00ff;
    x = (x ^ (x >> 8)) & 0x0000ffff;
    return x;
}

/* The glyphs are stored in a grid. Cell sizes vary between different font sizes.

   The grid is filled in a Morton order:
    1   2   5   6  17  18  21  22
    3   4   7   8  19  20  23  24
    9  10  13  14  25  26  29  30
   11  12  15  16  27  28  31  32
   33  34 ...
   ...
   i.e. we try to fill one small square, then three equal-sized squares so that we get one big square, etc...

   The glyphs are positioned around their baseline, which is located at y position glyph_size * i + tmAscent.
   Concerning the x position, the glyphs are centered around glyph_size * (i + 0.5). */
static HRESULT WINAPI ID3DXFontImpl_PreloadGlyphs(ID3DXFont *iface, UINT first, UINT last)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    UINT glyph, i, x, y;
    TEXTMETRICW tm;
    HRESULT hr;

    TRACE("iface %p, first %u, last %u\n", iface, first, last);

    if (last < first)
        return D3D_OK;

    ID3DXFont_GetTextMetricsW(iface, &tm);

    for (glyph = first; glyph <= last; glyph++)
    {
        DWORD ret, *pixel_data;
        BYTE *buffer;
        GLYPHMETRICS metrics;
        D3DLOCKED_RECT lockrect;
        MAT2 mat = { {0,1}, {0,0}, {0,0}, {0,1} };
        UINT stride, offx, offy;
        IDirect3DTexture9 *current_texture;
        struct d3dx_glyph *current_glyph;

        /* Check whether the glyph is already preloaded */
        for (i = 0; i < font->glyph_count; i++)
            if (font->glyphs[i].id == glyph)
                break;
        if (i < font->glyph_count)
            continue;

        /* Calculate glyph position */
        offx = morton_decode(font->texture_pos) * font->glyph_size;
        offy = morton_decode(font->texture_pos >> 1) * font->glyph_size;

        /* Make sure we have enough memory */
        if (font->glyph_count + 1 > font->allocated_glyphs)
        {
            struct d3dx_glyph *new_glyphs;
            UINT new_allocated_glyphs = font->allocated_glyphs << 1;

            new_glyphs = heap_realloc(font->glyphs, new_allocated_glyphs * sizeof(struct d3dx_glyph));
            if (!new_glyphs)
                return E_OUTOFMEMORY;

            font->glyphs = new_glyphs;
            font->allocated_glyphs = new_allocated_glyphs;
        }

        current_glyph = font->glyphs + font->glyph_count++;
        current_glyph->id = glyph;
        current_glyph->texture = NULL;

        /* Spaces are handled separately */
        if (glyph > 0 && glyph < 4)
            continue;

        /* Get the glyph data */
        ret = GetGlyphOutlineW(font->hdc, glyph, GGO_GLYPH_INDEX | GGO_GRAY8_BITMAP, &metrics, 0, NULL, &mat);
        if (ret == GDI_ERROR)
        {
            WARN("GetGlyphOutlineW failed\n");
            continue;
        }

        buffer = heap_alloc(ret);
        if (!buffer)
            return E_OUTOFMEMORY;

        GetGlyphOutlineW(font->hdc, glyph, GGO_GLYPH_INDEX | GGO_GRAY8_BITMAP, &metrics, ret, buffer, &mat);

        /* Create a new texture if necessary */
        if (font->texture_pos == font->glyphs_per_texture)
        {
            IDirect3DTexture9 **new_textures;
            UINT new_texture_count = font->texture_count + 1;

            new_textures = heap_realloc(font->textures, new_texture_count * sizeof(IDirect3DTexture9 *));
            if (!new_textures)
            {
                heap_free(buffer);
                return E_OUTOFMEMORY;
            }

            font->textures = new_textures;

            if (FAILED(hr = IDirect3DDevice9_CreateTexture(font->device, font->texture_size,
                                                           font->texture_size, 0, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED,
                                                           &font->textures[font->texture_count], NULL)))
            {
                heap_free(buffer);
                return hr;
            }

            font->texture_count++;
            font->texture_pos = 0;
            offx = 0;
            offy = 0;
        }

        current_texture = font->textures[font->texture_count - 1];

        /* Fill in glyph data */
        current_glyph->blackbox.left   = offx - metrics.gmptGlyphOrigin.x + font->glyph_size / 2 - (metrics.gmBlackBoxX + 3) / 2;
        current_glyph->blackbox.top    = offy - metrics.gmptGlyphOrigin.y + tm.tmAscent + 1;
        current_glyph->blackbox.right  = current_glyph->blackbox.left + metrics.gmBlackBoxX + 2;
        current_glyph->blackbox.bottom = current_glyph->blackbox.top + metrics.gmBlackBoxY + 2;
        current_glyph->cellinc.x       = metrics.gmptGlyphOrigin.x - 1;
        current_glyph->cellinc.y       = tm.tmAscent - metrics.gmptGlyphOrigin.y - 1;
        current_glyph->texture         = current_texture;

        /* Copy glyph data to the texture */
        if (FAILED(hr = IDirect3DTexture9_LockRect(current_texture, 0, &lockrect, &current_glyph->blackbox, 0)))
        {
            heap_free(buffer);
            return hr;
        }

        pixel_data = lockrect.pBits;
        stride = (metrics.gmBlackBoxX + 3) & ~3;
        for (y = 0; y < metrics.gmBlackBoxY; y++)
            for (x = 0; x < metrics.gmBlackBoxX; x++)
                pixel_data[x + y * lockrect.Pitch / 4] = buffer[x + y * stride] * 255 / 64 + 0xffffff00;

        IDirect3DTexture9_UnlockRect(current_texture, 0);

        heap_free(buffer);

        font->texture_pos++;
    }

    return D3D_OK;
}

static HRESULT WINAPI ID3DXFontImpl_PreloadTextA(ID3DXFont *iface, const char *string, INT count)
{

    WCHAR *wstr;
    HRESULT hr;
    INT countW;
    TRACE("iface %p, string %s, count %d\n", iface, debugstr_a(string), count);

    if (!string && count == 0) return D3D_OK;
    if (!string) return D3DERR_INVALIDCALL;

    countW = MultiByteToWideChar(CP_ACP, 0, string, count < 0 ? -1 : count, NULL, 0);

    wstr = heap_alloc(countW * sizeof(WCHAR));
    if (!wstr)
        return E_OUTOFMEMORY;

    MultiByteToWideChar(CP_ACP, 0, string, count < 0 ? -1 : count, wstr, countW);

    hr = ID3DXFont_PreloadTextW(iface, wstr, count);

    heap_free(wstr);

    return hr;
}

static HRESULT WINAPI ID3DXFontImpl_PreloadTextW(ID3DXFont *iface, const WCHAR *string, INT count)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    UINT i;
    WORD *indices;

    TRACE("iface %p, string %s, count %d\n", iface, debugstr_w(string), count);

    if (!string && count == 0) return D3D_OK;
    if (!string) return D3DERR_INVALIDCALL;
    if (count < 0) count = lstrlenW(string);

    indices = heap_alloc(count * sizeof(WORD));
    if (!indices)
        return E_OUTOFMEMORY;

    GetGlyphIndicesW(font->hdc, string, count, indices, 0);

    for (i = 0; i < count; i++)
        ID3DXFont_PreloadGlyphs(iface, indices[i], indices[i]);

    heap_free(indices);

    return D3D_OK;
}

static INT WINAPI ID3DXFontImpl_DrawTextA(ID3DXFont *iface, ID3DXSprite *sprite,
        const char *string, INT count, RECT *rect, DWORD format, D3DCOLOR color)
{
    FIXME("iface %p, sprite %p, string %s, count %d, rect %s, format %#x, color 0x%08x stub!\n",
            iface,  sprite, debugstr_a(string), count, wine_dbgstr_rect(rect), format, color);
    return 1;
}

static INT WINAPI ID3DXFontImpl_DrawTextW(ID3DXFont *iface, ID3DXSprite *sprite,
        const WCHAR *string, INT count, RECT *rect, DWORD format, D3DCOLOR color)
{
    FIXME("iface %p, sprite %p, string %s, count %d, rect %s, format %#x, color 0x%08x stub!\n",
            iface,  sprite, debugstr_w(string), count, wine_dbgstr_rect(rect), format, color);
    return 1;
}

static HRESULT WINAPI ID3DXFontImpl_OnLostDevice(ID3DXFont *iface)
{
    FIXME("iface %p stub!\n", iface);
    return D3D_OK;
}

static HRESULT WINAPI ID3DXFontImpl_OnResetDevice(ID3DXFont *iface)
{
    FIXME("iface %p stub\n", iface);
    return D3D_OK;
}

static const ID3DXFontVtbl D3DXFont_Vtbl =
{
    /*** IUnknown methods ***/
    ID3DXFontImpl_QueryInterface,
    ID3DXFontImpl_AddRef,
    ID3DXFontImpl_Release,
    /*** ID3DXFont methods ***/
    ID3DXFontImpl_GetDevice,
    ID3DXFontImpl_GetDescA,
    ID3DXFontImpl_GetDescW,
    ID3DXFontImpl_GetTextMetricsA,
    ID3DXFontImpl_GetTextMetricsW,
    ID3DXFontImpl_GetDC,
    ID3DXFontImpl_GetGlyphData,
    ID3DXFontImpl_PreloadCharacters,
    ID3DXFontImpl_PreloadGlyphs,
    ID3DXFontImpl_PreloadTextA,
    ID3DXFontImpl_PreloadTextW,
    ID3DXFontImpl_DrawTextA,
    ID3DXFontImpl_DrawTextW,
    ID3DXFontImpl_OnLostDevice,
    ID3DXFontImpl_OnResetDevice
};

HRESULT WINAPI D3DXCreateFontA(struct IDirect3DDevice9 *device, INT height, UINT width,
        UINT weight, UINT miplevels, BOOL italic, DWORD charset, DWORD precision, DWORD quality,
        DWORD pitchandfamily, const char *facename, struct ID3DXFont **font)
{
    D3DXFONT_DESCA desc;

    if( !device || !font ) return D3DERR_INVALIDCALL;

    desc.Height=height;
    desc.Width=width;
    desc.Weight=weight;
    desc.MipLevels=miplevels;
    desc.Italic=italic;
    desc.CharSet=charset;
    desc.OutputPrecision=precision;
    desc.Quality=quality;
    desc.PitchAndFamily=pitchandfamily;
    if(facename != NULL) lstrcpyA(desc.FaceName, facename);
    else desc.FaceName[0] = '\0';

    return D3DXCreateFontIndirectA(device, &desc, font);
}

HRESULT WINAPI D3DXCreateFontW(IDirect3DDevice9 *device, INT height, UINT width, UINT weight, UINT miplevels, BOOL italic, DWORD charset,
                               DWORD precision, DWORD quality, DWORD pitchandfamily, const WCHAR *facename, ID3DXFont **font)
{
    D3DXFONT_DESCW desc;

    if( !device || !font ) return D3DERR_INVALIDCALL;

    desc.Height=height;
    desc.Width=width;
    desc.Weight=weight;
    desc.MipLevels=miplevels;
    desc.Italic=italic;
    desc.CharSet=charset;
    desc.OutputPrecision=precision;
    desc.Quality=quality;
    desc.PitchAndFamily=pitchandfamily;
    if(facename != NULL) lstrcpyW(desc.FaceName, facename);
    else desc.FaceName[0] = '\0';

    return D3DXCreateFontIndirectW(device, &desc, font);
}

/***********************************************************************
 *           D3DXCreateFontIndirectA    (D3DX9_36.@)
 */
HRESULT WINAPI D3DXCreateFontIndirectA(IDirect3DDevice9 *device, const D3DXFONT_DESCA *desc, ID3DXFont **font)
{
    D3DXFONT_DESCW widedesc;

    if( !device || !desc || !font ) return D3DERR_INVALIDCALL;

    /* Copy everything but the last structure member. This requires the
       two D3DXFONT_DESC structures to be equal until the FaceName member */
    memcpy(&widedesc, desc, FIELD_OFFSET(D3DXFONT_DESCA, FaceName));
    MultiByteToWideChar(CP_ACP, 0, desc->FaceName, -1, widedesc.FaceName, ARRAY_SIZE(widedesc.FaceName));
    return D3DXCreateFontIndirectW(device, &widedesc, font);
}

/***********************************************************************
 *           D3DXCreateFontIndirectW    (D3DX9_36.@)
 */
HRESULT WINAPI D3DXCreateFontIndirectW(IDirect3DDevice9 *device, const D3DXFONT_DESCW *desc, ID3DXFont **font)
{
    D3DDEVICE_CREATION_PARAMETERS cpars;
    D3DDISPLAYMODE mode;
    struct d3dx_font *object;
    IDirect3D9 *d3d;
    TEXTMETRICW metrics;
    HRESULT hr;

    TRACE("(%p, %p, %p)\n", device, desc, font);

    if( !device || !desc || !font ) return D3DERR_INVALIDCALL;

    /* the device MUST support D3DFMT_A8R8G8B8 */
    IDirect3DDevice9_GetDirect3D(device, &d3d);
    IDirect3DDevice9_GetCreationParameters(device, &cpars);
    IDirect3DDevice9_GetDisplayMode(device, 0, &mode);
    hr = IDirect3D9_CheckDeviceFormat(d3d, cpars.AdapterOrdinal, cpars.DeviceType, mode.Format, 0, D3DRTYPE_TEXTURE, D3DFMT_A8R8G8B8);
    if (FAILED(hr))
    {
        IDirect3D9_Release(d3d);
        return D3DXERR_INVALIDDATA;
    }
    IDirect3D9_Release(d3d);

    object = heap_alloc_zero(sizeof(struct d3dx_font));
    if (!object)
    {
        *font = NULL;
        return E_OUTOFMEMORY;
    }
    object->ID3DXFont_iface.lpVtbl = &D3DXFont_Vtbl;
    object->ref = 1;
    object->device = device;
    object->desc = *desc;

    object->hdc = CreateCompatibleDC(NULL);
    if (!object->hdc)
    {
        heap_free(object);
        return D3DXERR_INVALIDDATA;
    }

    object->hfont = CreateFontW(desc->Height, desc->Width, 0, 0, desc->Weight, desc->Italic, FALSE, FALSE, desc->CharSet,
                                desc->OutputPrecision, CLIP_DEFAULT_PRECIS, desc->Quality, desc->PitchAndFamily, desc->FaceName);
    if (!object->hfont)
    {
        DeleteDC(object->hdc);
        heap_free(object);
        return D3DXERR_INVALIDDATA;
    }
    SelectObject(object->hdc, object->hfont);

    /* allocate common memory usage */
    object->allocated_glyphs = 32;
    object->glyphs = heap_alloc(object->allocated_glyphs * sizeof(struct d3dx_glyph));
    if (!object->glyphs)
    {
        DeleteObject(object->hfont);
        DeleteDC(object->hdc);
        heap_free(object);
        *font = NULL;
        return E_OUTOFMEMORY;
    }

    GetTextMetricsW(object->hdc, &metrics);

    object->glyph_size = make_pow2(metrics.tmHeight);

    object->texture_size = make_pow2(object->glyph_size);
    if (object->glyph_size < 256)
        object->texture_size = min(256, object->texture_size << 4);

    object->glyphs_per_texture = object->texture_size * object->texture_size / object->glyph_size / object->glyph_size;
    object->texture_pos = object->glyphs_per_texture;

    IDirect3DDevice9_AddRef(device);
    *font = &object->ID3DXFont_iface;

    return D3D_OK;
}
