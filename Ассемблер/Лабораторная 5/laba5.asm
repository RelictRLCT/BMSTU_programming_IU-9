assume cs:code, ds:data

;;;ВАРИАНТ 7

include macros.asm

data segment

stroka db 25, 0, 24 dup('$')
res db 25 dup(' ')
perevod db 0Dh, 0Ah, '$'

path db 'LABA5_IN.TXT',0
path2 db 'LABA5_OUT.TXT',0
filesize dw 25
handle dw ?
handle2 dw ?

data ends

code segment
begin:

push data
pop ds

xor al, al           ;
mov ah, 3dh          ;
mov dx, offset path  ; Открытие файла на чтение 
int 21h              ;
mov handle, ax       ;

xor cx, cx             ;
mov al, 1              ; 
mov ah, 3ch            ; Открытие файла на запись 
mov dx, offset path2   ;
int 21h                ;
mov handle2, ax        ;

mov ah, 3fh                  ;
mov bx, handle               ;
mov cx, filesize             ; Чтение строки
mov dx, offset stroka        ;
int 21h                      ;
        
mov cx, filesize

mov dx, offset stroka
mov bx, offset res
delete_space dx, bx, cx

mov ah, 40h             ;
mov bx, handle2         ;
mov cx, filesize        ; Запись результата в файл
mov dx, offset res      ;
int 21h                 ;

mov bx,handle2           ;
mov ah,3eh               ;
int 21h                  ;
                         ; Закрытие файлов
mov bx,handle            ;
mov ah,3eh               ;
int 21h                  ;

mov ah, 4ch
int 21h

code ends
end begin
