/*
 * Copyright 2000-2001 TransGaming Technologies Inc.
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef __WINE_DLLS_DDRAW_DDRAW_PRIVATE_H
#define __WINE_DLLS_DDRAW_DDRAW_PRIVATE_H

/* MAY NOT CONTAIN X11 or DGA specific includes/defines/structs! */

#include "winbase.h"
#include "wtypes.h"
#include "wingdi.h"
#include "winuser.h"
#include "ddraw.h"
#include "ddcomimpl.h"
#include "ddrawi.h"

/* XXX Put this somewhere proper. */
#define DD_STRUCT_INIT(x)			\
	do {					\
		memset((x), 0, sizeof(*(x)));	\
		(x)->dwSize = sizeof(*x);	\
	} while (0)

#define DD_STRUCT_COPY_BYSIZE(to,from)			\
	do {						\
	    	DWORD __size = (to)->dwSize;		\
	    	DWORD __copysize = __size;		\
                memset(to,0,__size);                    \
	        if ((from)->dwSize < __size) 		\
		    __copysize = (from)->dwSize;	\
		memcpy(to,from,__copysize);		\
		(to)->dwSize = __size;/*restore size*/	\
	} while (0)

/*****************************************************************************
 * IDirectDraw implementation structure
 */

typedef struct IDirectDrawImpl IDirectDrawImpl;
typedef struct IDirectDrawPaletteImpl IDirectDrawPaletteImpl;
typedef struct IDirectDrawClipperImpl IDirectDrawClipperImpl;
typedef struct IDirectDrawSurfaceImpl IDirectDrawSurfaceImpl;

typedef void (*pixel_convert_func)(void *src, void *dst, DWORD width,
				   DWORD height, LONG pitch,
				   IDirectDrawPaletteImpl *palette);

typedef void (*palette_convert_func)(LPPALETTEENTRY palent,
				     void *screen_palette, DWORD start,
				     DWORD count);

struct IDirectDrawImpl
{
    ICOM_VFIELD_MULTI(IDirectDraw7);
    ICOM_VFIELD_MULTI(IDirectDraw4);
    ICOM_VFIELD_MULTI(IDirectDraw2);
    ICOM_VFIELD_MULTI(IDirectDraw);

    DWORD ref;

    /* TRUE if created via DirectDrawCreateEx or CoCreateInstance,
     * FALSE if created via DirectDrawCreate. */
    BOOL ex;

    /* Linked list of surfaces, joined by next_ddraw in IDirectSurfaceImpl. */
    IDirectDrawSurfaceImpl* surfaces;
    /* Linked list of palettes, joined by next_ddraw. */
    IDirectDrawPaletteImpl* palettes;
    /* Linked list of clippers, joined by next_ddraw. */
    IDirectDrawClipperImpl* clippers;

    IDirectDrawSurfaceImpl* primary_surface;

    DDRAWI_DIRECTDRAW_LCL local;
    DDCAPS caps;

    HWND window;
    DWORD cooperative_level;
    WNDPROC original_wndproc;

    DWORD width, height;
    LONG pitch;
    DDPIXELFORMAT pixelformat;

    /* Should each of these go into some structure? */
    DWORD orig_width, orig_height;
    LONG orig_pitch;
    DDPIXELFORMAT orig_pixelformat;

    /* Called when the refcount goes to 0. */
    void (*final_release)(IDirectDrawImpl *This);

    HRESULT (*set_exclusive_mode)(IDirectDrawImpl *This, DWORD dwExcl);

    HRESULT (*create_palette)(IDirectDrawImpl* This, DWORD dwFlags,
			      LPDIRECTDRAWPALETTE* ppPalette,
			      LPUNKNOWN pUnkOuter);

    /* Surface creation functions. For all of these, pOuter == NULL. */

    /* Do not create any backbuffers or the flipping chain. */
    HRESULT (*create_primary)(IDirectDrawImpl* This,
			      const DDSURFACEDESC2* pDDSD,
			      LPDIRECTDRAWSURFACE7* ppSurf, LPUNKNOWN pOuter);

    /* Primary may be NULL if we are creating an unattached backbuffer. */
    HRESULT (*create_backbuffer)(IDirectDrawImpl* This,
				 const DDSURFACEDESC2* pDDSD,
				 LPDIRECTDRAWSURFACE7* ppSurf,
				 LPUNKNOWN pOuter,
				 IDirectDrawSurfaceImpl* primary);

    /* shiny happy offscreenplain surfaces */
    HRESULT (*create_offscreen)(IDirectDrawImpl* This,
				const DDSURFACEDESC2* pDDSD,
				LPDIRECTDRAWSURFACE7* ppSurf,
				LPUNKNOWN pOuter);

    /* dwMipMapLevel is specified as per OpenGL. (i.e. 0 is base) */
    HRESULT (*create_texture)(IDirectDrawImpl* This,
			      const DDSURFACEDESC2* pDDSD,
			      LPDIRECTDRAWSURFACE7* ppSurf, LPUNKNOWN pOuter,
			      DWORD dwMipMapLevel);

    HRESULT (*create_zbuffer)(IDirectDrawImpl* This,
			      const DDSURFACEDESC2* pDDSD,
			      LPDIRECTDRAWSURFACE7* ppSurf, LPUNKNOWN pOuter);

    LPVOID	private;

    /* Everything below here is still questionable. */

    DDPIXELFORMAT screen_pixelformat;

    int           pixmap_depth;
    pixel_convert_func pixel_convert;
    palette_convert_func palette_convert;

    /* Use to fool some too strict games */
    INT32 (*allocate_memory)(IDirectDrawImpl *This, DWORD mem);
    void (*free_memory)(IDirectDrawImpl *This, DWORD mem);
    DWORD total_vidmem, available_vidmem;
    
    /* This is to get the D3D object associated to this DDraw object */
    struct IDirect3DImpl *d3d;
    
    /* This is for the fake mainWindow */
    ATOM	winclass;
    PAINTSTRUCT	ps;
    BOOL	paintable;
};

/*****************************************************************************
 * IDirectDrawPalette implementation structure
 */
struct IDirectDrawPaletteImpl
{
    /* IUnknown fields */
    ICOM_VFIELD_MULTI(IDirectDrawPalette);
    DWORD ref;

    DDRAWI_DDRAWPALETTE_LCL local;
    DDRAWI_DDRAWPALETTE_GBL global;

    /* IDirectDrawPalette fields */
    HPALETTE		hpal;
    WORD		palVersion, palNumEntries; /* LOGPALETTE */
    PALETTEENTRY	palents[256];
    /* This is to store the palette in 'screen format' */
    int			screen_palents[256];

    VOID (*final_release)(IDirectDrawPaletteImpl* This);

    IDirectDrawImpl* ddraw_owner;
    IDirectDrawPaletteImpl* prev_ddraw;
    IDirectDrawPaletteImpl* next_ddraw;

    LPVOID		private;
};

/*****************************************************************************
 * IDirectDrawClipper implementation structure
 */
struct IDirectDrawClipperImpl
{
    /* IUnknown fields */
    ICOM_VFIELD_MULTI(IDirectDrawClipper);
    DWORD ref;

    /* IDirectDrawClipper fields */
    HWND hWnd;

    IDirectDrawImpl* ddraw_owner;
    IDirectDrawClipperImpl* prev_ddraw;
    IDirectDrawClipperImpl* next_ddraw;
};

/*****************************************************************************
 * IDirectDrawSurface implementation structure
 */

struct IDirectDrawSurfaceImpl
{
    /* IUnknown fields */
    ICOM_VFIELD_MULTI(IDirectDrawSurface7);
    ICOM_VFIELD_MULTI(IDirectDrawSurface3);
    ICOM_VFIELD_MULTI(IDirectDrawGammaControl);
    DWORD ref;

    struct IDirectDrawSurfaceImpl* attached; /* attached surfaces */

    struct IDirectDrawSurfaceImpl* next_ddraw; /* ddraw surface chain */
    struct IDirectDrawSurfaceImpl* prev_ddraw;
    struct IDirectDrawSurfaceImpl* next_attached; /* attached surface chain */
    struct IDirectDrawSurfaceImpl* prev_attached;

    IDirectDrawImpl* ddraw_owner;
    IDirectDrawSurfaceImpl* surface_owner;

    IDirectDrawPaletteImpl* palette; /* strong ref */
    IDirectDrawClipperImpl* clipper; /* strong ref */

    DDRAWI_DDRAWSURFACE_LCL local;
    DDRAWI_DDRAWSURFACE_MORE more;
    /* FIXME: since Flip should swap the GBL structures, they should
     * probably not be embedded into the IDirectDrawSurfaceImpl structure... */
    LPDDRAWI_DDRAWSURFACE_GBL_MORE gmore;
    DDRAWI_DDRAWSURFACE_GBL global;
    DDRAWI_DDRAWSURFACE_GBL_MORE global_more;

    DDSURFACEDESC2 surface_desc;

    HDC hDC;
    RECT lastlockrect;
    DWORD lastlocktype;
    BOOL dc_in_use;

    HRESULT (*duplicate_surface)(IDirectDrawSurfaceImpl* src,
				 LPDIRECTDRAWSURFACE7* dst);
    void (*final_release)(IDirectDrawSurfaceImpl *This);
    HRESULT (*late_allocate)(IDirectDrawSurfaceImpl *This);
    BOOL (*attach)(IDirectDrawSurfaceImpl *This, IDirectDrawSurfaceImpl *to);
    BOOL (*detach)(IDirectDrawSurfaceImpl *This);
    void (*lock_update)(IDirectDrawSurfaceImpl* This, LPCRECT pRect, DWORD dwFlags);
    void (*unlock_update)(IDirectDrawSurfaceImpl* This, LPCRECT pRect);
    void (*lose_surface)(IDirectDrawSurfaceImpl* This);
    BOOL (*flip_data)(IDirectDrawSurfaceImpl* front,
		      IDirectDrawSurfaceImpl* back,
		      DWORD dwFlags);
    void (*flip_update)(IDirectDrawSurfaceImpl* front, DWORD dwFlags);
    HRESULT (*get_dc)(IDirectDrawSurfaceImpl* This, HDC* phDC);
    HRESULT (*release_dc)(IDirectDrawSurfaceImpl* This, HDC hDC);
    void (*set_palette)(IDirectDrawSurfaceImpl* This, IDirectDrawPaletteImpl* pal);
    void (*update_palette)(IDirectDrawSurfaceImpl* This, IDirectDrawPaletteImpl* pal,
			   DWORD dwStart, DWORD dwCount, LPPALETTEENTRY palent);
    HWND (*get_display_window)(IDirectDrawSurfaceImpl *This);
    HRESULT (*get_gamma_ramp)(IDirectDrawSurfaceImpl *This, DWORD dwFlags, LPDDGAMMARAMP lpGammaRamp);
    HRESULT (*set_gamma_ramp)(IDirectDrawSurfaceImpl *This, DWORD dwFlags, LPDDGAMMARAMP lpGammaRamp);

    struct PrivateData* private_data;

    DWORD max_lod;
    DWORD priority;

    BOOL lost;

    DWORD uniqueness_value;

    LPVOID private;

    /* Everything below here is dodgy. */
    /* For Direct3D use */
    LPVOID aux_ctx, aux_data;
    void (*aux_release)(LPVOID ctx, LPVOID data);
    BOOL (*aux_flip)(LPVOID ctx, LPVOID data);
    void (*aux_unlock)(LPVOID ctx, LPVOID data, LPRECT lpRect);
    struct IDirect3DTextureImpl *texture;
    HRESULT (WINAPI *SetColorKey_cb)(struct IDirect3DTextureImpl *texture, DWORD dwFlags, LPDDCOLORKEY ckey ) ;
    /* This is to get the D3DDevice object associated to this surface */
    struct IDirect3DDeviceImpl *d3ddevice;
};

/*****************************************************************************
 * Driver initialisation functions.
 */
BOOL DDRAW_HAL_Init(HINSTANCE, DWORD, LPVOID);
BOOL DDRAW_User_Init(HINSTANCE, DWORD, LPVOID);

typedef struct {
    const DDDEVICEIDENTIFIER2* info;
    int	preference;	/* how good we are. dga might get 100, xlib 50*/
    HRESULT (*create)(const GUID*, LPDIRECTDRAW7*, LPUNKNOWN, BOOL ex);

    /* For IDirectDraw7::Initialize. */
    HRESULT (*init)(IDirectDrawImpl *, const GUID*);
} ddraw_driver;

void DDRAW_register_driver(const ddraw_driver*);

const ddraw_driver* DDRAW_FindDriver(const GUID* guid);

/******************************************************************************
 * Random utilities
 */

/* Get DDSCAPS of surface (shortcutmacro) */
#define SDDSCAPS(iface) ((iface)->s.surface_desc.ddsCaps.dwCaps)
/* Get the number of bytes per pixel for a given surface */
#define PFGET_BPP(pf) (pf.dwFlags&DDPF_PALETTEINDEXED8?1:((pf.u1.dwRGBBitCount+7)/8))
#define GET_BPP(desc) PFGET_BPP(desc.u4.ddpfPixelFormat)

LONG DDRAW_width_bpp_to_pitch(DWORD width, DWORD bpp);

typedef struct {
    unsigned short	bpp,depth;
    unsigned int	rmask,gmask,bmask;
} ConvertMode;

typedef struct {
    void (*pixel_convert)(void *src, void *dst, DWORD width, DWORD height, LONG pitch, IDirectDrawPaletteImpl* palette);
    void (*palette_convert)(LPPALETTEENTRY palent, void *screen_palette, DWORD start, DWORD count);
} ConvertFuncs;

typedef struct {
    ConvertMode screen, dest;
    ConvertFuncs funcs;
} Convert;

extern Convert ModeEmulations[8];
extern int _common_depth_to_pixelformat(DWORD depth,LPDIRECTDRAW ddraw);

/******************************************************************************
 * Structure conversion (for thunks)
 */
void DDRAW_Convert_DDSCAPS_1_To_2(const DDSCAPS* pIn, DDSCAPS2* pOut);
void DDRAW_Convert_DDDEVICEIDENTIFIER_2_To_1(const DDDEVICEIDENTIFIER2* pIn,
					     DDDEVICEIDENTIFIER* pOut);

/******************************************************************************
 * Debugging / Flags output functions
 */
extern void DDRAW_dump_DDBLTFX(DWORD flagmask);
extern void DDRAW_dump_DDBLTFAST(DWORD flagmask);
extern void DDRAW_dump_DDBLT(DWORD flagmask);
extern void DDRAW_dump_DDSCAPS(const DDSCAPS *in);
extern void DDRAW_dump_DDSCAPS2(const DDSCAPS2 *in);
extern void DDRAW_dump_pixelformat_flag(DWORD flagmask);
extern void DDRAW_dump_paletteformat(DWORD dwFlags);
extern void DDRAW_dump_pixelformat(const DDPIXELFORMAT *in);
extern void DDRAW_dump_colorkeyflag(DWORD ck);
extern void DDRAW_dump_surface_desc(const DDSURFACEDESC2 *lpddsd);
extern void DDRAW_dump_cooperativelevel(DWORD cooplevel);
extern void DDRAW_dump_lockflag(DWORD lockflag);
extern void DDRAW_dump_DDCOLORKEY(const DDCOLORKEY *in);
extern void DDRAW_dump_DDCAPS(const DDCAPS *lpcaps);

/* Used for generic dumping */
typedef struct
{
    DWORD val;
    const char* name;
} flag_info;

#define FE(x) { x, #x }

typedef struct
{
    DWORD val;
    const char* name;
    void (*func)(const void *);
    ptrdiff_t offset;
} member_info;

#define DDRAW_dump_flags(flags,names,num_names) DDRAW_dump_flags_(flags, names, num_names, 1)
#define ME(x,f,e) { x, #x, (void (*)(const void *))(f), offsetof(STRUCT, e) }

extern void DDRAW_dump_flags_(DWORD flags, const flag_info* names, size_t num_names, int newline);
extern void DDRAW_dump_members(DWORD flags, const void* data, const member_info* mems, size_t num_mems);

#endif /* __WINE_DLLS_DDRAW_DDRAW_PRIVATE_H */
