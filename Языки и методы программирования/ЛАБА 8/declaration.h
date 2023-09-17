#ifndef LAB7_DECLARATION_H
#define LAB7_DECLARATION_H
template <typename T, int N>
class Queue{
private:
    T* data = new T[N];
    int count =0;
    int cap = N;
    int head = 0;
    int tail = 0;
public:
    void insert(T elem){
        data[tail] = elem;
        tail +=1;
        count +=1;
    }

    T dequeue(){
        T x = data[head];
        head +=1;
        count -=1;
        return x;
    }

    bool empty(){
        return count == 0;
    }

    void perevernut(){
        T* data2 = new T[N];

        head = cap - head-1;
        if (head == cap-1){
            head = 0;
        }
        tail = cap - tail-1;

        for(int i=0; i<cap; i++){
            data2[i] = data[cap-i-1];
        }

        data = data2;
    }

};

template <int N>
class Queue<int, N>{
private:
    int su = 0;
    int* data = new int[N];
    int count =0;
    int cap = N;
    int head = 0;
    int tail = 0;
public:
    void insert(int elem){
        data[tail] = elem;
        tail +=1;
        count +=1;
        su += elem;
    }

    int dequeue(){
        int x = data[head];
        head +=1;
        if( head == cap){
            head = 0;
        }
        count -=1;
        su -= x;
        return x;
    }

    bool empty(){
        return count == 0;
    }

    void perevernut(){
        int* data2 = new int[N];

        head = cap - head - 1;
        if (head == cap-1){
            head = 0;
        }
        tail = cap - tail-1;

        for(int i=0; i<cap; i++){
            data2[i] = data[cap-i-1];
        }

        data = data2;

    }

    int sum(){
        return su;
    }
};
#endif
