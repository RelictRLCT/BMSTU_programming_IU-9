#ifndef LAB7_DECLARATION_H
#define LAB7_DECLARATION_H
int gcd(int a, int b);
int lcm(int a, int b);

class Vector {
public:
    Vector(int n);
    int razmer();
    class Drob{
    private:

    public:
        int chis;
        int znam;
        void dobavit_drob(int chis, int znam);
    };
    Drob& operator [] (int i);
    Drob* a;
    void vstavit(int i, int chis, int znam);
    Drob scalar();
    static Drob add(int ach, int bch, int azn, int bzn);
    Drob  mul(int ach, int bch, int azn, int bzn);
private:
    int n;
};
Vector::Drob reduce(Vector::Drob a);
#endif
