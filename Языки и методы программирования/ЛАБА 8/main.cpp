#include "iostream"
#include "declaration.h"
using namespace std;

int main(){
    Queue<int, 10> q;
    for(int i=0; i<10; i++){
        q.insert(i);
    }

    cout << q.sum() << endl;

    for(int i=0; i<10; i++){
        cout << q.dequeue() << " ";
    }

    cout << endl;

    for(int i=0; i<10; i++){
        q.insert(i);
    }

    cout << endl;

    q.perevernut();


    for(int i=0; i<10; i++){
        cout << q.dequeue() << " ";
    }

    cout << endl;
    Queue<double, 10> q2;
    for(int i=0; i<10; i++){
        q2.insert(1.123+i*2);
    }

    q2.perevernut();

    for(int i=0; i<10; i++){
        cout << q2.dequeue() << " ";
    }

}
