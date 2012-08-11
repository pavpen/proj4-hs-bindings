#include <stdio.h>

#include <proj_api.h>

#include <c_proj4.h>

projUV c_pj_fwd
(/* int64_t* restrict baseReg, int64_t* restrict sp, int64_t* restrict hp, */
 double u, double v, projPJ proj)
{
	projUV val;
	
	printf("Entered c_pj_fwd(u=%g, v=%g, proj=%p).\n", u, v, proj);
	val.u = u;
	val.v = v;
	val = pj_fwd(val, proj);
	return val;
}

projUV* c_pj_fwd_ptr(projUV *val, projPJ proj)
{
	projUV res;
	
	printf("Entered c_pj_fwd_ptr(proj=%p), *val={u=%g, v=%g}.\n", proj,
		val->u, val->v);
	res = pj_fwd(*val, proj);
	val->u = res.u;
	val->v = res.v;
}

projUV* c_pj_inv_ptr(projUV *val, projPJ proj)
{
	projUV res;
	
	printf("Entered c_pj_fwd_ptr(proj=%p).\n", proj);
	res = pj_inv(*val, proj);
	val->u = res.u;
	val->v = res.v;
}

CprojXYZ* c_pj_transform_pt
(projPJ src_cs, projPJ dst_cs, double x, double y, double z, CprojXYZ *res)
{
	printf("Entered c_pj_transform_pt(src_cs=%p, dst_cs=%p, x=%g, y=%g, z=%g, res=%p).\n",
		src_cs, dst_cs, x, y, z, res);
	res->x = x;
	res->y = y;
	res->z = z;
	res->errCode = pj_transform(src_cs, dst_cs, 1, 0,
		&res->x, &res->y, &res->z);
	printf("err_code=%i, x=%g, y=%g, z=%g.\n",
		res->errCode, res->x, res->y, res->z);
	return res;
}
