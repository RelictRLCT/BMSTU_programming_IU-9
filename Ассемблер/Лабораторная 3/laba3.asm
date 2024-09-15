assume cs: code, ds:data

;;;STRCSPN

data segment
perevod db 0Dh, 0Ah, '$'
str1 db 100, 0, 99 dup ('$')
sym db 100, 0, 99 dup ('$')
res db 4 dup(' ')
data ends

code segment
begin:

mov ax, data
mov ds, ax

push offset str1
call vvod

push offset sym
call vvod

push offset sym
push offset str1
call strcspn

call print

mov ah, 09h
mov dx, offset res
int 21h

mov ah, 4ch
int 21h

;----------Вывод результата
proc print
pop dx

pop ax
mov res[3], '$'
mov si, 2
print_loop:
mov bl, 10
div bl
add ah, '0'
mov res[si], ah
xor ah, ah

dec si
cmp al, 0
jne print_loop

push dx
ret
print endp

;----------Ввод строк
proc vvod

pop bx

pop dx
mov ah, 0Ah
int 21h
	
mov dx, offset perevod
mov ah, 09h
int 21h

push bx

ret
vvod endp

;-----------STRCSPN
proc strcspn
pop bx ;Сохранение адреса возврата

pop si
inc si ;Пропуск масимальной длины
mov cx, [ds:si] ;Получаю фактическую длину строки
xor ch, ch

pop di
inc di
mov ah, [ds:di]

inc si; Начало строк
inc di

push cx

check_loop:

	push di
	
	push ds
	pop es

	mov al, [ds:si]
	push cx
	
	
	xor cx, cx
	mov cl, ah
	repne scasb
	jz exit
	
	pop cx 

	pop di
	inc si
loop check_loop

jmp normal
exit:
pop cx
pop di
normal:
pop dx
sub dx, cx
push dx
push bx
ret
strcspn endp

code ends
end begin
