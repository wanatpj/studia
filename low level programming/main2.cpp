#include "D.hpp"
#include <cstdio>

//Natural::Natural(){
//	while(true);
//}

//Natural::Natural(unsigned long int A){
//while(true);
//}

//Natural::Natural(const char *A){
//while(true);
//}

//Natural::Natural(const Natural &A){
//while(true);
//}

//Natural::~Natural(){
//	while(true);
//}

//Natural& Natural::operator=(const Natural &A){
//}
	
//Natural& Natural::operator+=(const Natural &A){
//while(true);
//}

//Natural& Natural::operator++(){
//	return (*this)+=Natural(1);
//}

//Natural& Natural::operator*=(const Natural &A){
//}
	
//bool Natural::operator==(const Natural &A) const{
//	return true;
//}

//bool Natural::operator<(const Natural &A) const{
//	return false;
//}
	
//unsigned long int Natural::Size() const{
//	return dl*8;
//}

//void Natural::Print() const{
//	if(dl == 0) printf("0");
//	else{
//		printf("zaalokowane: %d\n",dl);
//		printf("%x",wsk[dl-1]);
//		for(int i=dl-2;i>=0;i--) printf("%.2x",wsk[i]);
//	}
//}

int main(){
	Natural *a = new Natural("102");
	a->Print();
	printf("\n");
	Natural b = *a;
	b.Print();
	printf("\n");
	a = new Natural(257);
	b.Print();
	printf("\n");
	a->Print();
	printf("\n");
	printf("a-size: %lu b-size: %lu\n",a->Size(),b.Size());
	Natural A((unsigned long int)1023);
	Natural B((unsigned long int)1);
	printf("A-size: %lu B-size: %lu\n",A.Size(),B.Size());
	printf("A < B: %d B < A: %d B == A: %d\n",A<B,B<A,A==B);
	
	A+=B+=B;
	A.Print();
	printf("\n");
	++A;
	A.Print();
	printf("\n");
	Natural C((unsigned long int)255);
	++C;
	C.Print();
	printf("\n");
	Natural D("21");
	D.Print();
	printf("\n");
	D = C = A+=(++A);
	D.Print();
	printf("\n");
	Natural ZERO;
	D*=ZERO;
	D.Print();
	printf("\n");
	Natural E((unsigned long int)123456);
	Natural F((unsigned long int)98765);
	E*=F;
	E.Print();
	printf("\n");
	Natural G((unsigned long int)2);
	Natural H("ffffffffffffffff3412321786127836127836781263127389271938712893712983712893791823791287391823791823791827319827391873");
	H*=H*=H*=H;
	H.Print();
	printf("\n");
	G.Print();
	printf("\n");
	G*=H*=G;
	G.Print();
	printf("\n");
	Natural Z(G);
	++++Z;
	Z.Print();
	printf("\n");
	Natural Y;
	Y.Print();
	printf("\n");
	++Y;
	Y.Print();
	printf("\n");
	printf("G: "); G.Print(); printf("\n");
	printf("H: "); H.Print(); printf("\n");
	printf("C-size: %lu D-size: %lu\n",C.Size(),D.Size());
	printf("C < D: %d D < C: %d C == D: %d\n",C<D,D<C,C==D);
	printf("E-size: %lu F-size: %lu\n",E.Size(),F.Size());
	printf("E < F: %d F < E: %d E == F: %d\n",E<F,F<E,E==F);
	printf("G-size: %lu H-size: %lu\n",G.Size(),H.Size());
	printf("G < H: %d H < G: %d G == H: %d\n",G<H,H<G,G==H);
	printf("Z-size: %lu Y-size: %lu\n",Z.Size(),Y.Size());
	printf("Z < Y: %d Y < Z: %d Z == Y: %d\n",Z<Y,Y<Z,Z==Y);
}
