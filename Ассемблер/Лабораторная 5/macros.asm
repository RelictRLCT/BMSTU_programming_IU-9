delete_space macro str_adr, res_adr, len_str
push cx
push si
push di

mov cx, len_str
mov si, str_adr
mov di, res_adr

delete_loop:
mov al, [si]
cmp al, ' '
je not_print
mov [di], al
inc di 
not_print:
inc si

loop delete_loop

pop di
pop si
pop cx
endm


out_str macro str

push ax
push dx
mov ah, 09h
mov dx, str
int 21h
pop dx
pop ax

endm
