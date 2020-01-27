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

#include <assert.h>

#include "usp10.h"

WINE_DEFAULT_DEBUG_CHANNEL(d3dx);

struct d3dx_glyph
{
    UINT id;
    RECT blackbox;
    POINT cellinc;
    IDirect3DTexture9 *texture;

    struct wine_rb_entry entry;
};

struct d3dx_font
{
    ID3DXFont ID3DXFont_iface;
    LONG ref;

    IDirect3DDevice9 *device;
    D3DXFONT_DESCW desc;

    HDC hdc;
    HFONT hfont;

    struct wine_rb_tree glyph_tree;

    IDirect3DTexture9 **textures;
    UINT texture_count, texture_pos;

    UINT texture_size, glyph_size, glyphs_per_texture;
};

static int glyph_rb_compare(const void *key, const struct wine_rb_entry *entry)
{
    struct d3dx_glyph *glyph = WINE_RB_ENTRY_VALUE(entry, struct d3dx_glyph, entry);
    const UINT *id = key;

    return *id - glyph->id;
}

static void glyph_rb_free(struct wine_rb_entry *entry, void *context)
{
    struct d3dx_glyph *glyph = WINE_RB_ENTRY_VALUE(entry, struct d3dx_glyph, entry);

    heap_free(glyph);
}

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

        wine_rb_destroy(&font->glyph_tree, glyph_rb_free, NULL);

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
    struct wine_rb_entry *entry;
    struct d3dx_glyph *current_glyph;
    HRESULT hr;

    TRACE("iface %p, glyph %#x, texture %p, blackbox %p, cellinc %p\n",
          iface, glyph, texture, blackbox, cellinc);

    hr = ID3DXFont_PreloadGlyphs(iface, glyph, glyph);
    if (FAILED(hr))
        return hr;

    entry = wine_rb_get(&font->glyph_tree, &glyph);
    if (entry)
    {
        current_glyph = WINE_RB_ENTRY_VALUE(entry, struct d3dx_glyph, entry);
        if (cellinc)
            *cellinc = current_glyph->cellinc;
        if (blackbox)
            *blackbox = current_glyph->blackbox;
        if (texture)
            *texture = current_glyph->texture;
        if (texture && *texture)
            IDirect3DTexture9_AddRef(current_glyph->texture);
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
    UINT glyph, x, y;
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
        if (wine_rb_get(&font->glyph_tree, &glyph))
            continue;

        /* Calculate glyph position */
        offx = morton_decode(font->texture_pos) * font->glyph_size;
        offy = morton_decode(font->texture_pos >> 1) * font->glyph_size;

        current_glyph = heap_alloc(sizeof(struct d3dx_glyph));
        if (!current_glyph)
            return E_OUTOFMEMORY;

        current_glyph->id = glyph;
        current_glyph->texture = NULL;
        wine_rb_put(&font->glyph_tree, &current_glyph->id, &current_glyph->entry);

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
    WCHAR *wstr;
    INT ret, countW;

    TRACE("iface %p, sprite %p, string %s, count %d, rect %s, format %#x, color 0x%08x\n",
          iface,  sprite, debugstr_a(string), count, wine_dbgstr_rect(rect), format, color);

    if (!string || count == 0)
        return 0;

    countW = MultiByteToWideChar(CP_ACP, 0, string, count < 0 ? -1 : count, NULL, 0);

    if (countW == 0)
        return 0;

    wstr = heap_alloc_zero(countW * sizeof(WCHAR));
    if (!wstr)
        return 0;

    MultiByteToWideChar(CP_ACP, 0, string, count < 0 ? -1 : count, wstr, countW);

    ret = ID3DXFont_DrawTextW(iface, sprite, wstr, count, rect, format, color);

    heap_free(wstr);

    return ret;
}

/* DrawText helpers copied from user32 */
#define TAB     9
#define LF     10
#define CR     13
#define SPACE  32
static void TEXT_WordBreak(HDC hdc, WCHAR *str, unsigned int max_str,
                           unsigned int *len_str,
                           int width, int format, unsigned int chars_fit,
                           unsigned int *chars_used, SIZE *size)
{
    WCHAR *p;
    BOOL word_fits;
    SCRIPT_LOGATTR *sla;
    SCRIPT_ANALYSIS sa;
    int i;

    assert(format & DT_WORDBREAK);
    assert(chars_fit < *len_str);

    sla = heap_alloc(sizeof(SCRIPT_LOGATTR) * *len_str);

    memset(&sa, 0, sizeof(SCRIPT_ANALYSIS));
    sa.eScript = SCRIPT_UNDEFINED;

    ScriptBreak(str, *len_str, &sa, sla);

    /* Work back from the last character that did fit to either a space or the
     * last character of a word, whichever is met first.
     */
    p = str + chars_fit; /* The character that doesn't fit */
    i = chars_fit;
    word_fits = TRUE;
    if (!chars_fit)
        word_fits = FALSE;
    else if (sla[i].fSoftBreak) /* chars_fit < *len_str so this is valid */
    {
        /* the word just fitted */
        p--;
    }
    else
    {
        while (i > 0 && !sla[(--i)+1].fSoftBreak) p--;
        p--;
        word_fits = (i != 0 || sla[i+1].fSoftBreak);
    }

    /* If there was one. */
    if (word_fits)
    {
        BOOL next_is_space;
        /* break the line before/after that character */
        if (!(format & (DT_RIGHT | DT_CENTER)) || *p != SPACE)
            p++;
        next_is_space = (p - str) < *len_str && *p == SPACE;
        *len_str = p - str;
        /* and if the next character is a space then discard it. */
        *chars_used = *len_str;
        if (next_is_space)
            (*chars_used)++;
    }
    /* Suppose there was none. */
    else
    {
        /* discard any trailing space. */
        const WCHAR *e = str + *len_str;
        p = str + chars_fit;
        while (p < e && *p != SPACE)
            p++;
        *chars_used = p - str;
        if (p < e) /* i.e. loop failed because *p == SPACE */
            (*chars_used)++;
        *len_str = p - str;
    }
    /* Remeasure the string */
    GetTextExtentExPointW(hdc, str, *len_str, 0, NULL, NULL, size);
    heap_free(sla);
}

static const WCHAR *TEXT_NextLineW(HDC hdc, const WCHAR *str, int *count,
                                   WCHAR *dest, int *len, int width, DWORD format,
                                   SIZE *retsize, int last_line, int tabwidth)
{
    int i = 0, j = 0;
    int plen = 0;
    SIZE size;
    int maxl = *len;
    int seg_i, seg_count, seg_j;
    int max_seg_width;
    int num_fit;
    BOOL word_broken, line_fits;
    unsigned int j_in_seg;

    /* For each text segment in the line */

    retsize->cy = 0;
    while (*count)
    {

        /* Skip any leading tabs */

        if (str[i] == TAB && (format & DT_EXPANDTABS))
        {
            plen = ((plen/tabwidth)+1)*tabwidth;
            (*count)--; if (j < maxl) dest[j++] = str[i++]; else i++;
            while (*count && str[i] == TAB)
            {
                plen += tabwidth;
                (*count)--; if (j < maxl) dest[j++] = str[i++]; else i++;
            }
        }


        /* Now copy as far as the next tab or cr/lf or eos */

        seg_i = i;
        seg_count = *count;
        seg_j = j;

        while (*count && (str[i] != TAB || !(format & DT_EXPANDTABS)) && ((str[i] != CR && str[i] != LF) || (format & DT_SINGLELINE)))
        {
            (*count)--;
            if (j < maxl) dest[j++] = str[i];
            i++;
        }

        /* Measure the whole text segment and possibly WordBreak */

        j_in_seg = j - seg_j;
        max_seg_width = width - plen;
        GetTextExtentExPointW(hdc, dest + seg_j, j_in_seg, max_seg_width, &num_fit, NULL, &size);

        /* The Microsoft handling of various combinations of formats is weird.
         * The following may very easily be incorrect if several formats are
         * combined, and may differ between versions (to say nothing of the
         * several bugs in the Microsoft versions).
         */
        word_broken = FALSE;
        line_fits = (num_fit >= j_in_seg);
        if (!line_fits && (format & DT_WORDBREAK))
        {
            const WCHAR *s;
            unsigned int chars_used;
            TEXT_WordBreak(hdc, dest+seg_j, maxl-seg_j, &j_in_seg,
                           max_seg_width, format, num_fit, &chars_used, &size);
            line_fits = (size.cx <= max_seg_width);
            /* and correct the counts */
            *count = seg_count - chars_used;
            s = str + seg_i + chars_used;
            i = s - str;
            word_broken = TRUE;
        }

        j = seg_j + j_in_seg;

        plen += size.cx;
        if (size.cy > retsize->cy)
            retsize->cy = size.cy;

        if (word_broken)
            break;
        else if (!*count)
            break;
        else if (str[i] == CR || str[i] == LF)
        {
            (*count)--, i++;
            if (*count && (str[i] == CR || str[i] == LF) && str[i] != str[i-1])
            {
                (*count)--, i++;
            }
            break;
        }
        /* else it was a Tab and we go around again */
    }

    retsize->cx = plen;
    *len = j;
    if (*count)
        return (&str[i]);
    else
        return NULL;
}

#define MAX_BUFFER 1024
static INT WINAPI ID3DXFontImpl_DrawTextW(ID3DXFont *iface, ID3DXSprite *sprite,
        const WCHAR *string, INT count, RECT *rect, DWORD format, D3DCOLOR color)
{
    struct d3dx_font *font = impl_from_ID3DXFont(iface);
    ID3DXSprite *target = sprite;

    const WCHAR *strPtr;
    WCHAR line[MAX_BUFFER];
    int lh;
    TEXTMETRICW tm;
    int x, y;
    int width;
    int max_width = 0;
    int last_line;
    int tabwidth = 0;
    RECT textrect = {0};

    TRACE("iface %p, sprite %p, string %s, count %d, rect %s, format %#x, color 0x%08x\n",
          iface,  sprite, debugstr_w(string), count, wine_dbgstr_rect(rect), format, color);

    if (!string)
        return 0;

    if (count < 0)
        count = lstrlenW(string);

    if (count == 0)
        return 0;

    if (format & DT_SINGLELINE)
        format &= ~DT_WORDBREAK;
    if (format & DT_CALCRECT)
        format |= DT_NOCLIP;

    if (!rect)
    {
        y = ID3DXFont_DrawTextW(iface, NULL, string, count, &textrect, format | DT_CALCRECT, 0);

        if (format & DT_CALCRECT)
            return y;
    }
    else
        textrect = *rect;

    x = textrect.left;
    y = textrect.top;
    width = textrect.right - textrect.left;
    strPtr = string;

    ID3DXFont_GetTextMetricsW(iface, &tm);
    lh = tm.tmHeight;

    if (format & DT_EXPANDTABS)
        tabwidth = tm.tmAveCharWidth * 8;

    if (!(format & DT_CALCRECT) && !sprite)
    {
        D3DXCreateSprite(font->device, &target);
        ID3DXSprite_Begin(target, 0);
    }

    do {
        SIZE size;
        int len = ARRAY_SIZE(line);

        last_line = !(format & DT_NOCLIP) && (y + lh > textrect.bottom);
        strPtr = TEXT_NextLineW(font->hdc, strPtr, &count, line, &len, width, format, &size, last_line, tabwidth);

        if (format & DT_CENTER)
            x = (textrect.left + textrect.right - size.cx) / 2;
        else if (format & DT_RIGHT)
            x = textrect.right - size.cx;

        if (format & DT_SINGLELINE)
        {
            if (format & DT_VCENTER)
                y = textrect.top + (textrect.bottom - textrect.top) / 2 - size.cy / 2;
            else if (format & DT_BOTTOM)
                y = textrect.bottom - size.cy;
        }

        if (!(format & DT_CALCRECT))
        {
            int xseg = x;
            const WCHAR *str = line;

            while (len)
            {
                int len_seg;
                GCP_RESULTSW results;
                D3DXVECTOR3 pos;
                UINT i;

                if ((format & DT_EXPANDTABS))
                {
                    const WCHAR *p;
                    p = str; while (p < str+len && *p != TAB) p++;
                    len_seg = p - str;
                    if (len_seg != len && !GetTextExtentPointW(font->hdc, str, len_seg, &size))
                        return 0;
                }
                else
                    len_seg = len;

                ZeroMemory(&results, sizeof(GCP_RESULTSW));
                results.lpCaretPos = heap_alloc(len_seg * sizeof(INT));
                results.lpGlyphs = heap_alloc(len_seg * sizeof(WORD));
                results.nGlyphs = len_seg;

                GetCharacterPlacementW(font->hdc, str, len_seg, 0, &results, 0);

                for (i = 0; i < results.nGlyphs; i++)
                {
                    LPDIRECT3DTEXTURE9 tex;
                    RECT bbox;
                    POINT cinc;

                    ID3DXFont_GetGlyphData(iface, results.lpGlyphs[i], &tex, &bbox, &cinc);

                    if (!tex)
                        continue;

                    pos.x = results.lpCaretPos[i] + cinc.x + xseg;
                    pos.y = cinc.y + y;

                    ID3DXSprite_Draw(target, tex, &bbox, NULL, &pos, color);
                    IDirect3DTexture9_Release(tex);
                }

                len -= len_seg;
                str += len_seg;
                if (len)
                {
                    assert((format & DT_EXPANDTABS) && *str == TAB);
                    len--; str++;
                    xseg += ((size.cx/tabwidth)+1)*tabwidth;
                }
            }
        }
        else if (size.cx > max_width)
            max_width = size.cx;

        y += lh;
    } while (strPtr && !last_line);

    textrect.right = textrect.left + max_width;
    textrect.bottom = y;

    if ((format & DT_CALCRECT) && rect)
        *rect = textrect;

    if (target != sprite)
    {
        ID3DXSprite_End(target);
        ID3DXSprite_Release(target);
    }

    return y - textrect.top;
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

    wine_rb_init(&object->glyph_tree, glyph_rb_compare);

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
