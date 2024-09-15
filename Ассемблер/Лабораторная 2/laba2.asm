assume cs:code, ds:data
                                                                               ;ВАРИАНТ 3
data segment
arr1 dw 3, -3, 5, -2, 1
arr2 dw 2, 4, 1, 19, 0
res dw 0
mes db "      $"
data ends

code segment
begin:

mov ax, data
mov ds, ax

xor si, si 
mov cx, 5 

scalar_mul:
xor ax, ax
mov ax, word ptr arr1[si]
mov bx, word ptr arr2[si]
imul bx
add res, ax
inc si
inc si
loop scalar_mul


xor si, si
cmp res, 0
jg plus
mov mes[si], '-'
neg res
plus:


mov si, 5
mov cx, 5
mov ax, [res]

print:
xor dx, dx
mov bx, 10
idiv bx
add dx, '0'
mov mes[si], byte ptr dl
dec si
loop print

mov ah, 09h
mov dl, offset mes
int 21h

mov ah, 4ch
int 21h

code ends
end begin
