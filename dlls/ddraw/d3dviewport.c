/* Direct3D Viewport
 * Copyright (c) 1998 Lionel ULMER
 *
 * This file contains the implementation of Direct3DViewport2.
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

#include "config.h"
#include "windef.h"
#include "winerror.h"
#include "wine/obj_base.h"
#include "ddraw.h"
#include "d3d.h"
#include "wine/debug.h"

#include "d3d_private.h"
#include "mesa_private.h"

WINE_DEFAULT_DEBUG_CHANNEL(ddraw);

static void activate(IDirect3DViewportImpl* This) {
    IDirect3DLightImpl* light;

    /* Activate all the lights associated with this context */
    light = This->lights;

    while (light != NULL) {
        light->activate(light);
	light = light->next;
    }
}


static void _dump_D3DVIEWPORT(D3DVIEWPORT *lpvp)
{
    TRACE("    - dwSize = %ld   dwX = %ld   dwY = %ld\n",
	  lpvp->dwSize, lpvp->dwX, lpvp->dwY);
    TRACE("    - dwWidth = %ld   dwHeight = %ld\n",
	  lpvp->dwWidth, lpvp->dwHeight);
    TRACE("    - dvScaleX = %f   dvScaleY = %f\n",
	  lpvp->dvScaleX, lpvp->dvScaleY);
    TRACE("    - dvMaxX = %f   dvMaxY = %f\n",
	  lpvp->dvMaxX, lpvp->dvMaxY);
    TRACE("    - dvMinZ = %f   dvMaxZ = %f\n",
	  lpvp->dvMinZ, lpvp->dvMaxZ);
}

static void _dump_D3DVIEWPORT2(D3DVIEWPORT2 *lpvp)
{
    TRACE("    - dwSize = %ld   dwX = %ld   dwY = %ld\n",
	  lpvp->dwSize, lpvp->dwX, lpvp->dwY);
    TRACE("    - dwWidth = %ld   dwHeight = %ld\n",
	  lpvp->dwWidth, lpvp->dwHeight);
    TRACE("    - dvClipX = %f   dvClipY = %f\n",
	  lpvp->dvClipX, lpvp->dvClipY);
    TRACE("    - dvClipWidth = %f   dvClipHeight = %f\n",
	  lpvp->dvClipWidth, lpvp->dvClipHeight);
    TRACE("    - dvMinZ = %f   dvMaxZ = %f\n",
	  lpvp->dvMinZ, lpvp->dvMaxZ);
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_QueryInterface(LPDIRECT3DVIEWPORT3 iface,
                                                REFIID riid,
                                                LPVOID* obp)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    TRACE("(%p/%p)->(%s,%p)\n", This, iface, debugstr_guid(riid), obp);

    *obp = NULL;

    if ( IsEqualGUID(&IID_IUnknown,  riid) ||
	 IsEqualGUID(&IID_IDirect3DViewport, riid) ||
	 IsEqualGUID(&IID_IDirect3DViewport2, riid) ||
	 IsEqualGUID(&IID_IDirect3DViewport3, riid) ) {
        IDirect3DViewport3_AddRef(ICOM_INTERFACE(This, IDirect3DViewport3));
        *obp = ICOM_INTERFACE(This, IDirect3DViewport3);
	TRACE("  Creating IDirect3DViewport1/2/3 interface %p\n", *obp);
	return S_OK;
    }
    FIXME("(%p): interface for IID %s NOT found!\n", This, debugstr_guid(riid));
    return OLE_E_ENUM_NOMORE;
}

ULONG WINAPI
Main_IDirect3DViewportImpl_3_2_1_AddRef(LPDIRECT3DVIEWPORT3 iface)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->() incrementing from %lu.\n", This, iface, This->ref);
    return ++(This->ref);
}

ULONG WINAPI
Main_IDirect3DViewportImpl_3_2_1_Release(LPDIRECT3DVIEWPORT3 iface)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->() decrementing from %lu.\n", This, iface, This->ref);
    if (!--(This->ref)) {
        HeapFree(GetProcessHeap(), 0, This);
	return 0;
    }
    return This->ref;
}


HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_Initialize(LPDIRECT3DVIEWPORT3 iface,
                                            LPDIRECT3D lpDirect3D)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    TRACE("(%p/%p)->(%p) no-op...\n", This, iface, lpDirect3D);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_GetViewport(LPDIRECT3DVIEWPORT3 iface,
                                             LPD3DVIEWPORT lpData)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    DWORD dwSize;
    TRACE("(%p/%p)->(%p)\n", This, iface, lpData);
    if (This->use_vp2 != 0) {
        ERR("  Requesting to get a D3DVIEWPORT struct where a D3DVIEWPORT2 was set !\n");
	return DDERR_INVALIDPARAMS;
    }
    dwSize = lpData->dwSize;
    memset(lpData, 0, dwSize);
    memcpy(lpData, &(This->viewports.vp1), dwSize);

    if (TRACE_ON(ddraw)) {
        TRACE("  returning D3DVIEWPORT :");
	_dump_D3DVIEWPORT(lpData);
    }
    
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_SetViewport(LPDIRECT3DVIEWPORT3 iface,
                                             LPD3DVIEWPORT lpData)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    TRACE("(%p/%p)->(%p)\n", This, iface, lpData);

    if (TRACE_ON(ddraw)) {
        TRACE("  getting D3DVIEWPORT :\n");
	_dump_D3DVIEWPORT(lpData);
    }

    This->use_vp2 = 0;
    memset(&(This->viewports.vp1), 0, sizeof(This->viewports.vp1));
    memcpy(&(This->viewports.vp1), lpData, lpData->dwSize);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_TransformVertices(LPDIRECT3DVIEWPORT3 iface,
                                                   DWORD dwVertexCount,
                                                   LPD3DTRANSFORMDATA lpData,
                                                   DWORD dwFlags,
                                                   LPDWORD lpOffScreen)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%08lx,%p,%08lx,%p): stub!\n", This, iface, dwVertexCount, lpData, dwFlags, lpOffScreen);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_LightElements(LPDIRECT3DVIEWPORT3 iface,
                                               DWORD dwElementCount,
                                               LPD3DLIGHTDATA lpData)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%08lx,%p): stub!\n", This, iface, dwElementCount, lpData);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_SetBackground(LPDIRECT3DVIEWPORT3 iface,
                                               D3DMATERIALHANDLE hMat)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%08lx): stub!\n", This, iface, (DWORD) hMat);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_GetBackground(LPDIRECT3DVIEWPORT3 iface,
                                               LPD3DMATERIALHANDLE lphMat,
                                               LPBOOL lpValid)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%p,%p): stub!\n", This, iface, lphMat, lpValid);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_SetBackgroundDepth(LPDIRECT3DVIEWPORT3 iface,
                                                    LPDIRECTDRAWSURFACE lpDDSurface)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%p): stub!\n", This, iface, lpDDSurface);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_GetBackgroundDepth(LPDIRECT3DVIEWPORT3 iface,
                                                    LPDIRECTDRAWSURFACE* lplpDDSurface,
                                                    LPBOOL lpValid)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%p,%p): stub!\n", This, iface, lplpDDSurface, lpValid);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_Clear(LPDIRECT3DVIEWPORT3 iface,
                                       DWORD dwCount,
                                       LPD3DRECT lpRects,
                                       DWORD dwFlags)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%08lx,%p,%08lx): stub!\n", This, iface, dwCount, lpRects, dwFlags);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_AddLight(LPDIRECT3DVIEWPORT3 iface,
                                          LPDIRECT3DLIGHT lpDirect3DLight)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    IDirect3DLightImpl *lpDirect3DLightImpl = ICOM_OBJECT(IDirect3DLightImpl, IDirect3DLight, lpDirect3DLight);
    
    TRACE("(%p/%p)->(%p)\n", This, iface, lpDirect3DLight);
    
    /* Add the light in the 'linked' chain */
    lpDirect3DLightImpl->next = This->lights;
    This->lights = lpDirect3DLightImpl;

    /* If active, activate the light */
    if (This->active_device != NULL) {
        lpDirect3DLightImpl->activate(lpDirect3DLightImpl);
    }
    
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_DeleteLight(LPDIRECT3DVIEWPORT3 iface,
                                             LPDIRECT3DLIGHT lpDirect3DLight)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    IDirect3DLightImpl *lpDirect3DLightImpl = ICOM_OBJECT(IDirect3DLightImpl, IDirect3DLight, lpDirect3DLight);
    IDirect3DLightImpl *cur_light, *prev_light = NULL;
    
    TRACE("(%p/%p)->(%p)\n", This, iface, lpDirect3DLight);
    cur_light = This->lights;
    while (cur_light != NULL) {
        if (cur_light == lpDirect3DLightImpl) {
	    lpDirect3DLightImpl->desactivate(lpDirect3DLightImpl);
	    if (prev_light == NULL) This->lights = cur_light->next;
	    else prev_light->next = cur_light->next;
	    return DD_OK;
	}
	prev_light = cur_light;
	cur_light = cur_light->next;
    }
    return DDERR_INVALIDPARAMS;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_1_NextLight(LPDIRECT3DVIEWPORT3 iface,
                                           LPDIRECT3DLIGHT lpDirect3DLight,
                                           LPDIRECT3DLIGHT* lplpDirect3DLight,
                                           DWORD dwFlags)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%p,%p,%08lx): stub!\n", This, iface, lpDirect3DLight, lplpDirect3DLight, dwFlags);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_GetViewport2(LPDIRECT3DVIEWPORT3 iface,
                                            LPD3DVIEWPORT2 lpData)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    DWORD dwSize;
    TRACE("(%p/%p)->(%p)\n", This, iface, lpData);
    if (This->use_vp2 != 1) {
        ERR("  Requesting to get a D3DVIEWPORT2 struct where a D3DVIEWPORT was set !\n");
	return DDERR_INVALIDPARAMS;
    }
    dwSize = lpData->dwSize;
    memset(lpData, 0, dwSize);
    memcpy(lpData, &(This->viewports.vp2), dwSize);

    if (TRACE_ON(ddraw)) {
        TRACE("  returning D3DVIEWPORT2 :");
	_dump_D3DVIEWPORT2(lpData);
    }
    
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_2_SetViewport2(LPDIRECT3DVIEWPORT3 iface,
                                            LPD3DVIEWPORT2 lpData)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    TRACE("(%p/%p)->(%p)\n", This, iface, lpData);

    if (TRACE_ON(ddraw)) {
        TRACE("  getting D3DVIEWPORT2 :\n");
	_dump_D3DVIEWPORT2(lpData);
    }

    This->use_vp2 = 1;
    memset(&(This->viewports.vp2), 0, sizeof(This->viewports.vp2));
    memcpy(&(This->viewports.vp2), lpData, lpData->dwSize);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_SetBackgroundDepth2(LPDIRECT3DVIEWPORT3 iface,
                                                 LPDIRECTDRAWSURFACE4 lpDDS)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%p): stub!\n", This, iface, lpDDS);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_GetBackgroundDepth2(LPDIRECT3DVIEWPORT3 iface,
                                                 LPDIRECTDRAWSURFACE4* lplpDDS,
                                                 LPBOOL lpValid)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%p,%p): stub!\n", This, iface, lplpDDS, lpValid);
    return DD_OK;
}

HRESULT WINAPI
Main_IDirect3DViewportImpl_3_Clear2(LPDIRECT3DVIEWPORT3 iface,
                                    DWORD dwCount,
                                    LPD3DRECT lpRects,
                                    DWORD dwFlags,
                                    DWORD dwColor,
                                    D3DVALUE dvZ,
                                    DWORD dwStencil)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    FIXME("(%p/%p)->(%08lx,%p,%08lx,%08lx,%f,%08lx): stub!\n", This, iface, dwCount, lpRects, dwFlags, dwColor, dvZ, dwStencil);
    return DD_OK;
}

HRESULT WINAPI
GL_IDirect3DViewportImpl_3_2_1_Clear(LPDIRECT3DVIEWPORT3 iface,
                                       DWORD dwCount,
                                       LPD3DRECT lpRects,
                                       DWORD dwFlags)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    GLboolean ztest;
    
    TRACE("(%p/%p)->(%08lx,%p,%08lx)\n", This, iface, dwCount, lpRects, dwFlags);

    if (dwCount != 1) {
        WARN("  Warning, this function only for now clears the whole screen...\n");
    }
    
    /* Clears the screen */
    ENTER_GL();
    glGetBooleanv(GL_DEPTH_WRITEMASK, &ztest);
    glDepthMask(GL_TRUE); /* Enables Z writing to be sure to delete also the Z buffer */
    glClear(((dwFlags & D3DCLEAR_TARGET) ? GL_COLOR_BUFFER_BIT : 0) |
	    ((dwFlags & D3DCLEAR_ZBUFFER) ? GL_DEPTH_BUFFER_BIT : 0));
    glDepthMask(ztest);
    LEAVE_GL();
    
    return DD_OK;
}

HRESULT WINAPI
GL_IDirect3DViewportImpl_3_Clear2(LPDIRECT3DVIEWPORT3 iface,
                                    DWORD dwCount,
                                    LPD3DRECT lpRects,
                                    DWORD dwFlags,
                                    DWORD dwColor,
                                    D3DVALUE dvZ,
                                    DWORD dwStencil)
{
    ICOM_THIS_FROM(IDirect3DViewportImpl, IDirect3DViewport3, iface);
    GLboolean ztest;
    GLfloat old_z_clear_value;
    GLbitfield bitfield = 0;
    GLint old_stencil_clear_value;
    GLfloat old_color_clear_value[4];
    
    TRACE("(%p/%p)->(%08lx,%p,%08lx,%08lx,%f,%08lx)\n", This, iface, dwCount, lpRects, dwFlags, dwColor, dvZ, dwStencil);

    if (dwCount != 1) {
        WARN("  Warning, this function only for now clears the whole screen...\n");
    }

    /* Clears the screen */
    ENTER_GL();
    if (dwFlags & D3DCLEAR_ZBUFFER) {
        glGetBooleanv(GL_DEPTH_WRITEMASK, &ztest);
	glDepthMask(GL_TRUE); /* Enables Z writing to be sure to delete also the Z buffer */
	glGetFloatv(GL_DEPTH_CLEAR_VALUE, &old_z_clear_value);
	glClearDepth(dvZ);
	TRACE(" Depth value : %f\n", dvZ);
	bitfield |= GL_DEPTH_BUFFER_BIT;
    }
    if (dwFlags & D3DCLEAR_STENCIL) {
        bitfield |= GL_STENCIL_BUFFER_BIT;
	glGetIntegerv(GL_STENCIL_CLEAR_VALUE, &old_stencil_clear_value);
	glClearStencil(dwStencil);
	TRACE(" Stencil value : %ld\n", dwStencil);
    }    
    if (dwFlags & D3DCLEAR_TARGET) {
        bitfield |= GL_COLOR_BUFFER_BIT;
	glGetFloatv(GL_COLOR_CLEAR_VALUE, old_color_clear_value);
	glClearColor(((dwColor >> 16) & 0xFF) / 255.0,
		     ((dwColor >>  8) & 0xFF) / 255.0,
		     ((dwColor >>  0) & 0xFF) / 255.0,
		     ((dwColor >> 24) & 0xFF) / 255.0);
	TRACE("Color value (ARGB) : %08lx\n", dwColor);
    }
    
    glClear(bitfield);
    
    if (dwFlags & D3DCLEAR_ZBUFFER) {
        glDepthMask(ztest);
	glClearDepth(old_z_clear_value);
    }
     if (dwFlags & D3DCLEAR_STENCIL) {
        bitfield |= GL_STENCIL_BUFFER_BIT;
	glClearStencil(old_stencil_clear_value);
    }    
    if (dwFlags & D3DCLEAR_TARGET) {
        bitfield |= GL_COLOR_BUFFER_BIT;
	glClearColor(old_color_clear_value[0],
		     old_color_clear_value[1],
		     old_color_clear_value[2],
		     old_color_clear_value[3]);
    }
    
    LEAVE_GL();
    
    return DD_OK;
}

#if !defined(__STRICT_ANSI__) && defined(__GNUC__)
# define XCAST(fun)     (typeof(VTABLE_IDirect3DViewport3.fun))
#else
# define XCAST(fun)     (void*)
#endif

ICOM_VTABLE(IDirect3DViewport3) VTABLE_IDirect3DViewport3 =
{
    ICOM_MSVTABLE_COMPAT_DummyRTTIVALUE
    XCAST(QueryInterface) Main_IDirect3DViewportImpl_3_2_1_QueryInterface,
    XCAST(AddRef) Main_IDirect3DViewportImpl_3_2_1_AddRef,
    XCAST(Release) Main_IDirect3DViewportImpl_3_2_1_Release,
    XCAST(Initialize) Main_IDirect3DViewportImpl_3_2_1_Initialize,
    XCAST(GetViewport) Main_IDirect3DViewportImpl_3_2_1_GetViewport,
    XCAST(SetViewport) Main_IDirect3DViewportImpl_3_2_1_SetViewport,
    XCAST(TransformVertices) Main_IDirect3DViewportImpl_3_2_1_TransformVertices,
    XCAST(LightElements) Main_IDirect3DViewportImpl_3_2_1_LightElements,
    XCAST(SetBackground) Main_IDirect3DViewportImpl_3_2_1_SetBackground,
    XCAST(GetBackground) Main_IDirect3DViewportImpl_3_2_1_GetBackground,
    XCAST(SetBackgroundDepth) Main_IDirect3DViewportImpl_3_2_1_SetBackgroundDepth,
    XCAST(GetBackgroundDepth) Main_IDirect3DViewportImpl_3_2_1_GetBackgroundDepth,
    XCAST(Clear) GL_IDirect3DViewportImpl_3_2_1_Clear,
    XCAST(AddLight) Main_IDirect3DViewportImpl_3_2_1_AddLight,
    XCAST(DeleteLight) Main_IDirect3DViewportImpl_3_2_1_DeleteLight,
    XCAST(NextLight) Main_IDirect3DViewportImpl_3_2_1_NextLight,
    XCAST(GetViewport2) Main_IDirect3DViewportImpl_3_2_GetViewport2,
    XCAST(SetViewport2) Main_IDirect3DViewportImpl_3_2_SetViewport2,
    XCAST(SetBackgroundDepth2) Main_IDirect3DViewportImpl_3_SetBackgroundDepth2,
    XCAST(GetBackgroundDepth2) Main_IDirect3DViewportImpl_3_GetBackgroundDepth2,
    XCAST(Clear2) GL_IDirect3DViewportImpl_3_Clear2,
};

#if !defined(__STRICT_ANSI__) && defined(__GNUC__)
#undef XCAST
#endif




HRESULT d3dviewport_create(IDirect3DViewportImpl **obj, IDirect3DImpl *d3d)
{
    IDirect3DViewportImpl *object;

    object = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(IDirect3DViewportImpl));
    if (object == NULL) return DDERR_OUTOFMEMORY;

    object->ref = 1;
    object->d3d = d3d;
    object->activate = activate;
    object->use_vp2 = 0xFF;
    object->next = NULL;
    object->lights = NULL;
    
    ICOM_INIT_INTERFACE(object, IDirect3DViewport3, VTABLE_IDirect3DViewport3);

    *obj = object;

    TRACE(" creating implementation at %p.\n", *obj);
    
    return D3D_OK;
}
