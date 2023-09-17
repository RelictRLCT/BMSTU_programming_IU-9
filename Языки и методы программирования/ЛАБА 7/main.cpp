#include "iostream"
#include "declaration.h"
using namespace std;

int main(){
    Vector V = *new Vector{4};
    for(int i = 0; i< V.razmer(); ++i){
        cout << V[i].chis << "/" << V[i].znam;
        cout << endl;
    }
    cout << "Размер: " << V.razmer() << endl;

    V.vstavit(1, 3, 5);
    cout << endl;
    for(int i = 0; i< V.razmer(); ++i){
        cout << V[i].chis << "/" << V[i].znam;
        cout << endl;
    }
    cout << "Размер: " << V.razmer() << endl;

    Vector::Drob sum = Vector::add(V[0].chis, V[1].chis, V[0].znam, V[1].znam);
    cout << sum.chis << "/" << sum.znam;
    cout << endl;
    Vector::Drob scal =  V.scalar();
    cout << scal.chis << "/" << scal.znam;
}
