#include "iostream"
#include "declaration.h"
using namespace std;

Vector::Vector(int n) {
    this->n = n;
    a = new Drob[n];

    for (int i = 0; i<n; ++i){
        int ch, zn;
        cout << "Введите числитель и знаменатель дроби: ";
        cin >> ch;
        cin >> zn;
        a[i].dobavit_drob(ch, zn);
    }
}

int Vector::razmer() {
    return n;
}

Vector::Drob& Vector::operator[](int i) {
    return a[i];
};

void Vector::Drob::dobavit_drob(int ch, int zn) {
        this->chis = ch;
        this->znam = zn;
        while (gcd(chis, znam) != 1) {
            int g = gcd(chis, znam);
            chis /= g;
            znam /= g;
        }
}

::ostream& operator<< (::ostream& os , Vector &vec ){
    for ( int i = 0 ; i < vec.razmer(); i ++) {
        os << vec[i].chis << "/" << vec[i].znam;
        os << endl ;
    }
    return os ;
}

void Vector::vstavit(int i, int chis, int znam){
    n = n+1;
    Drob*  b = new Drob[n];
    for(int j=0; j<i+1; j++){
        b[j]=a[j];
    }
    b[i+1].dobavit_drob(chis, znam);
    for(int j=i+2; j<n; j++){
        b[j] = a[j-1];
    }
    a = b;
    cout << "Элемент вставлен";
}

Vector::Drob Vector::scalar() {
    Drob sum = {0, 1};
    for(int i=0; i<n; i++){
        sum = add(sum.chis, mul(a[i].chis, a[i].chis, a[i].znam, a[i].znam).chis
        , sum.znam, mul(a[i].chis, a[i].chis, a[i].znam, a[i].znam).znam);
    }
    return sum;
}

Vector::Drob Vector::add(int ach, int bch, int azn, int bzn) {
    Drob c;
    int znam = lcm(azn, bzn);
    c.znam = znam;
    c.chis = ach * (znam / azn) + bch * (znam / bzn);
    c = reduce(c);
    return c;
}

Vector::Drob Vector::mul(int ach, int bch, int azn, int bzn) {
    Drob c;
    int znam = azn * bzn;
    c.znam = znam;
    c.chis = ach * bch;
    c = reduce(c);
    return c;
}

int gcd(int a, int b) {
    a = abs(a);
    b = abs(b);
    int c;
    while (b) {
        c = a % b;
        a = b;
        b = c;
    }
    return abs(a);
}

int lcm(int a, int b) {
    return abs((a*b))/gcd(a,b);
}

Vector::Drob reduce(Vector::Drob a){
    while (gcd(a.chis, a.znam) != 1) {
        int g = gcd(a.chis, a.znam);
        a.chis /= g;
        a.znam /= g;
    }
    return a;
}
