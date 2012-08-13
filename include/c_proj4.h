#include <proj_api.h>


typedef struct {
	double x;
	double y;
	double z;
	int    errCode;
} CprojXYZ;


projUV c_pj_fwd(double u, double v, projPJ proj);
projUV* c_pj_fwd_ptr(projUV* val, projPJ proj);

projUV* c_pj_inv_ptr(projUV* val, projPJ proj);

CprojXYZ* c_pj_transform_pt
(projPJ src_cs, projPJ dst_cs, double x, double y, double z, CprojXYZ *res);


#ifdef _DEBUG

void c_pj_free(projPJ proj);

#endif /* DEBUG */
