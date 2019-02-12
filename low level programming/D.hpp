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
//Natural::Natural(){}
