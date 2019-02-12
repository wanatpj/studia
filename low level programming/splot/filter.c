#include "filter.h"
void filter(struct RGBImage * dst, const struct RGBImage * src, const float * matrix){
	float f[9];
	long siz = 3*src->width*src->height;
	unsigned int width = 3*src->width;
	int i;
	float ff;
	for(i=siz;i>=0;--i){
		ff = 0;
		ff += src->data[i+3+width] * f[8];
		ff += src->data[i+0+width] * f[7];
		ff += src->data[i-3+width] * f[6];
		ff += src->data[i+3] * f[5];
		ff += src->data[i+0] * f[4];
		ff += src->data[i-3] * f[3];
		ff += src->data[i+3-width] * f[2];
		ff += src->data[i+0-width] * f[1];
		ff += src->data[i-3-width] * f[0];
		dst->data[i] = (unsigned char)ff;
	}
}

