#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "filter.h"

int main(int argc, char * argv[])
{
	FILE * f;

	struct RGBImage src;
	struct RGBImage dst;
	
	float m[16];
	int i;
	
	char buf[4096];
	
	fprintf(stderr, "Current working directory: %s\n", getcwd(buf, 4096));
	fprintf(stderr, "argv[1] = %s\n", argv[1]);
	
	uint32_t maxcolor;
	
	if (argc < 2)
		return 0;
		
	fprintf(stderr, "OK\n");
		
	if ((f = fopen(argv[1], "rb")) == NULL)
	{
		fprintf(stderr, "Cannot read matrix.\n");
		return 0;
	}
	
	for (i = 0; i < 9; ++i)
		fscanf(f, "%f", &(m[i]));
	
	fclose(f);
	
	fprintf(stderr, "Macierz\n");
	for (i = 0; i < 9; ++i)
		fprintf(stderr, "%f ", m[i]);
	fprintf(stderr, "\n");
	
	scanf("P6 %u %u %u", &(src.width), &(src.height), &maxcolor);
	fgetc(stdin);
	src.data = (uint8_t *)malloc(3*src.width*src.height);
	fread(src.data, 3*src.width, src.height, stdin);
	
	fprintf(stderr, "WIDTH: %u\nHEIGHT: %u\nMAXCOLOR: %u\n", src.width, src.height, maxcolor);
	
	dst.width = src.width;
	dst.height = src.height;
	dst.data = (uint8_t *)malloc(3*src.width*src.height);
	for (i = 0; i < 1; ++i){
		//fprintf(stderr, "Conv num: %d\n", i + 1);
		filter(&dst, &src, m);
	}
	
	printf("P6\n%u %u\n255\n", dst.width, dst.height);
	fwrite(dst.data, 3*dst.width, dst.height, stdout);
	
	free(src.data);
	free(dst.data);
	
	return 0;
}
