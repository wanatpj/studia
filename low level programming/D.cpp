#include<cstdio>
#include<cstdlib>
#include<cstring>
class Natural{
	long bits;
	long * con;
public:
	Natural();
	Natural(unsigned long int);
	Natural(const char *);
	Natural(const Natural &);
	~Natural();
	Natural & operator=(const Natural &);
	Natural & operator+=(const Natural &);
	Natural & operator++();
	Natural & operator*=(const Natural &);
	bool operator==(const Natural &) const;
	bool operator<(const Natural &) const;
	unsigned long int Size() const;
	void Print() const;
};
Natural::Natural(){}
Natural::Natural(unsigned long int n){}
Natural::Natural(const char * str){
/*	int len = strlen(str);
	con = (long*)malloc((len+1)/2);
	for(int i=0; i<(len+1)/2; ++i)
		con[i] =
		 ((int)str[2*i]<<4)|
		str[2*i+1];*/
}
Natural::Natural(const Natural & A){ }
Natural::~Natural(){}
Natural & Natural::operator=(const Natural & A){ }
Natural & Natural::operator+=(const Natural & A){
	int a = bits>=A.bits?bits:A.bits;
	con = (long*)realloc(con, 8*a);
	for(int i=bits; i<A.bits; ++i) con[i] = A.con[i];
	a = bits<=A.bits?bits:A.bits;
	for(int i=0; i<a; ++i) con[i] += A.con[i];
	return *this;
}
Natural & Natural::operator++(){}
Natural & Natural::operator*=(const Natural & A){}
bool Natural::operator==(const Natural & A) const{}
bool Natural::operator<(const Natural & A) const{}
unsigned long int Natural::Size() const{}
void Natural::Print() const{}
int main(){
	Natural n;
	Natural m = Natural(n);
//	m = Natural(n);
}

