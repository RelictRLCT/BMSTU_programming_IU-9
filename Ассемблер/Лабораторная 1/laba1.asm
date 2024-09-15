assume cs:code, ds:data
;ВАРИАНТ 3
data segment
a db 14
b db 8
c db 3
d db 4
res db ?
mes db '000$'
data ends

code segment
begin:

mov ax, data
mov ds, ax

shl a, 1

mov cl, 2
shr b, cl

mov al, a
sub al, [b]

mov bl, [c]
mul bl

sub al, [d]
mov res, al




xor ah, ah
mov cl, 10
div cl

xor si, si
inc si
inc si

mov mes[si], ah
add mes[si], '0'

dec si
mov cl, 10
xor ah, ah
div cl

mov mes[si], ah
add mes[si], '0'

dec si
mov cl, 10
xor ah, ah
div cl

mov mes[si], ah
add mes[si], '0'

mov dx, offset mes
mov ah, 09h
int 21h

;
mov dl, 0Dh  ;
mov ah, 02h  ;
int 21h      ;Перевод строки
mov dl, 0Ah  ;
mov ah, 02h  ;
int 21h      ;
;

xor ah, ah
mov cl, 16
mov al, [res]
div cl

xor si, si
inc si
inc si

cmp ah, 9
jle label1

sub ah, 10
add ah, 'A'
mov mes[si], ah

jmp label2
label1:

mov mes[si], ah
add mes[si], '0'

label2:


dec si
mov cl, 16
xor ah, ah
div cl

cmp ah, 9
jle label3

sub ah, 10
add ah, 'A'
mov mes[si], ah

jmp label4
label3:
mov mes[si], ah
add mes[si], '0'

label4:
dec si
mov cl, 16
xor ah, ah
div cl

cmp ah, 9
jle label5

sub ah, 10
add ah, 'A'
mov mes[si], ah

jmp label6
label5:
mov mes[si], ah
add mes[si], '0'
label6:

mov dx, offset mes
mov ah, 09h
int 21h

;mov ah, 02h   ;
;mov dl, [res] ;
;add dl, '0'   ;
;int 21h       ;

mov ah, 4ch
int 21h

code ends
end begin
