.model small
assume cs:code, ds:data

data segment
perevod db 0Dh, 0Ah, '$'
tip_chisel db ?
tip_operacii db ?
perenos db 0
prigl1 db "Vvedite tip chisla: 0 - dec, 1 - hex", 0Dh, 0Ah, '$'
prigl2 db "Vvedite tip operacii: 0 - bezznak plus, 1 - bezznak minus, 2 - bezznak mul, 3 - znak plus, 4 - znak mul", 0Dh, 0Ah, '$'
err_mes db "Nepravilny vvod$"
max_raz db 11
chis1 db 25, 0, 25 dup('$')
chis2 db 25, 0, 25 dup('$')
chislo1 db 25 dup('$')
chislo2 db 25 dup('$')
chislo_to_mul db 25 dup('$')
minus_odin db 1, 24 dup('$')
res db ' ', 99 dup('$')
res_zero db "0$"
lenstr_si dw 0
lenstr_di dw 0
ret_adress dw ?
ret_adress_n dw ?
si_adress dw ?
di_adress dw ?
si_addr_bm dw ?
di_addr_bm dw ?
was_not_zero dw 0
zaem_counter dw 0
data ends

code segment
begin:
push data
pop ds

mov dx, offset prigl1 ;
mov ah, 09h           ; Вывод приглашения
int 21h               ;

mov ah, 10h           ;
int 16h               ; Ввод символа

mov ah, 02h ;
mov dl, al  ;Вывод на экран введённого символа
int 21h     ;

mov tip_chisel, al

mov dx, offset perevod
mov ah, 09h
int 21h

mov dx, offset prigl2 ;
mov ah, 09h           ; Вывод приглашения
int 21h               ;

mov ah, 10h           ;
int 16h               ; Ввод символа

mov ah, 02h ;
mov dl, al  ;Вывод на экран введённого символа
int 21h     ;

mov tip_operacii, al

mov dx, offset perevod
mov ah, 09h
int 21h


push offset chislo1
push offset chis1
call vvod
push offset chislo2
push offset chis2
call vvod

cmp tip_chisel, '1'
je call_bezznak_plus_hex_jmp
jmp call_bezznak_plus_hex_jmp_not
call_bezznak_plus_hex_jmp:
jmp call_bezznak_plus_hex
call_bezznak_plus_hex_jmp_not:

;---------------------------ВЫБОР ФУНКЦИИ-------------------
cmp tip_operacii, '0'
je call_bezznak_plus

cmp tip_operacii, '1'
je call_bezznak_minus

cmp tip_operacii, '2'
je call_bezznak_mul

cmp tip_operacii, '3'
je call_znak_plus

cmp tip_operacii, '4'
je call_znak_mul

jmp exit

jmp normal_exit
call_bezznak_plus:
push offset chislo2
push offset chislo1
call bezznak_plus            ; Беззнаковое сложение
jmp normal_exit

call_bezznak_minus:
push offset chislo2
push offset chislo1
call bezznak_minus           ; Беззнаковое вычитание
jmp normal_exit

call_bezznak_mul:
push offset chislo2
push offset chislo1
call bezznak_mul            ; Беззнаковое умножение
jmp normal_exit

call_znak_plus:
push offset chislo2
push offset chislo1
call znak_plus              ; Знаковое сложение
jmp normal_exit

call_znak_mul:
push offset chislo2
push offset chislo1
call znak_mul             ; Знаковое вычитание
jmp normal_exit

call_bezznak_plus_hex:
push offset chislo2
push offset chislo1
call bezznak_plus_hex       ; 16-ричное беззнаковое сложение
jmp normal_exit

exit:

mov ah, 09h             ;
mov dx, offset err_mes  ; Вывод сообщения об ошибке
int 21h                 ;

jmp end_pr
normal_exit:

call print

end_pr:
mov ah, 4ch
int 21h

;----------------ВВОД----------------
proc vvod
pop bx

pop dx              ; Адрес chis
mov si, dx          ; Сохранил адрес начала строки для цикла проверки
mov ah, 0Ah
int 21h


mov dx, offset perevod ;
mov ah, 09h            ; Перевод строки
int 21h                ;

pop di                 ; Адрес новой строки с цифрами

inc si
mov cx, [ds:si] ;Получаю фактическую длину строки
xor ch, ch
mov dl, cl      ;Сохранил длину
inc si

cmp tip_chisel, '1'
je proverka_hex

proverka_loop:

mov ah, [ds:si]

cmp ah, '-'
je minus

sub ah, '0'
cmp ah, 9
jg exit

jmp norm

minus:
cmp cl, dl            ;Если минус посреди числа
jne exit
cmp tip_operacii, '3' ;Если операция беззнаковая
jl exit

norm:
mov ds:di, ah

inc si
inc di

loop proverka_loop
push bx
ret

proverka_hex:

proverka_hex_loop:

mov ah, [ds:si]

cmp ah, 'A'
jge letters_hex

sub ah, '0'
cmp ah, 9
jg exit

jmp norm_hex

letters_hex:
cmp ah, 'F'
jg exit

sub ah, 'A'
add ah, 10

norm_hex:
mov ds:di, ah

inc si
inc di

loop proverka_hex_loop
push bx
ret
vvod endp

;-----------------ВЫВОД РЕЗУЛЬТАТА----------------
proc print
pop bx

pop di
xor si, si
mov cx, 27

;cmp perenos, 1
;jne print_loop
;mov res[si+1], '1'
;inc si

cmp tip_chisel, '1'
je print_hex_loop

print_loop:

mov al, [ds:di]
cmp al, '$'
je exit_print

mov res[si+1], al
add res[si+1], '0'
inc si
inc di
loop print_loop
exit_print:
mov dx, offset res
mov ah, 09h
int 21h

push bx
ret

print_hex_loop:
mov al, [ds:di]
cmp al, '$'
je exit_print

cmp al, 10
jge print_letter
mov res[si+1], al
add res[si+1], '0'
jmp next_sym
print_letter:
sub al, 10
mov res[si+1], al
add res[si+1], 'A'
next_sym:
inc si
inc di
loop print_hex_loop

push bx
ret
print endp

;---------------------------------БЕЗЗНАКОВОЕ СЛОЖЕНИЕ----------------------
proc bezznak_plus
pop bx

pop si   ;
pop di   ; Адреса начала чисел

push bx

push si
call to_end_str

push di
call to_end_str
pop di
pop ax   ; Длина строки di
pop si
pop cx   ; Длина строки si

pop bx

cmp cx, ax
jge cx_big_bezzplus

xor cx, ax
xor ax, cx
xor cx, ax

xor si, di
xor di, si
xor si, di

cx_big_bezzplus:

mov lenstr_si, cx
mov lenstr_di, ax

; В si - длинная строка, в cx - ее длина
bezzplus_loop:

mov al, [perenos]
add al, [si]
mov [si], al
mov perenos, 0

cmp lenstr_di, 0
je end_di_bezzplus

mov al, [di]
add al, [si]

cmp al, 9
jle not_perenos_bezznplus
sub al, 10
mov perenos, 1
not_perenos_bezznplus:
mov [si], al

dec si
dec di
dec lenstr_si
dec lenstr_di

loop bezzplus_loop
jmp norm_ex_bp
end_di_bezzplus:

mov al, [perenos]
add al, [si]
mov [si], al
mov perenos, 0

mov al, [si]
cmp al, 9
jle not_perenos_bezzplus_end
sub al, 10
mov perenos, 1
not_perenos_bezzplus_end:
mov [si], al
dec si
dec lenstr_si
loop bezzplus_loop
norm_ex_bp:

mov al, perenos
cmp al, 1
je sdvig_si
jmp not_sdvig_si
sdvig_si:
mov perenos, 0
inc si
mov dx, si
mov al, [si]
inc si
sdvig_loop_plus:
mov ah, [si]
cmp ah, '$'
je exit_sdvig_plus
mov [si], al
mov al, ah
inc si
jmp sdvig_loop_plus
exit_sdvig_plus:
mov [si], al
mov al, '$'
mov [si+1], al

mov si, dx
mov al, 1
mov [si], al

push si
push bx
ret

not_sdvig_si:
inc si
push si
push bx
ret
bezznak_plus endp

;-----------------------------БЕЗЗНАКОВОЕ ВЫЧИТАНИЕ----------------------
proc bezznak_minus
pop bx

pop si   ;
pop di   ; Адреса начала чисел

;mov si_addr_bm, si
;mov di_addr_bm, di

push bx

push si
call skip_zeros
pop si

push di
call skip_zeros
pop di

pop bx

push si ; Сохраняю для дальнейшей проверки правильности
push di ;

push bx
push si
call to_end_str

push di
call to_end_str
pop di
pop ax   ; Длина строки di
pop si
pop cx   ; Длина строки si

mov lenstr_si, cx
mov lenstr_di, ax

pop bx

cmp cx, ax
jl jmp_exit_bm

jmp not_jmp_bm
jmp_exit_bm:
jmp exit
not_jmp_bm:

cmp cx, ax
jg bezzminus_corr

pop ax
pop dx

push si     ;
push di     ; Адреса последних символов

mov si, dx
mov di, ax

proverka_bezzminus_loop:

mov al, [si]
cmp al, [di]
jg bezzminus_is_correct

cmp al, [di]
jl jmp_exit_bm

inc si
inc di
loop proverka_bezzminus_loop

jmp bezzminus_corr
bezzminus_is_correct:
pop di
pop si

bezzminus_corr: ;--------Начало вычитания. В si - большее
pop cx
pop cx

mov cx, lenstr_si

bezzminus_loop:
mov al, [si]
cmp al, [di]
jge bezzmin_sub

zaem_loop:                               ;Цикл поиска ненулевого элемента
dec si
inc zaem_counter
mov ah, [si]
cmp ah, 0
je zaem_loop
mov ah, [si]
sub ah, 1
mov [si], ah

zaem_dobavka_nine_loop:
inc si
cmp zaem_counter, 1
jg plus_nine
add al, 10
jmp exit_zaem
plus_nine:
mov ah, 9
mov [si], ah
dec zaem_counter
jmp zaem_dobavka_nine_loop
exit_zaem:
mov zaem_counter, 0


bezzmin_sub:
sub al, [di]
mov [si], al
dec si
dec di
dec lenstr_si
dec lenstr_di

cmp lenstr_di, 0
je exit_bezzm_loop

loop bezzminus_loop
exit_bezzm_loop:


to_start_si_bm_loop:
cmp lenstr_si, 0
jg to_start_step
jmp end_bm
to_start_step:
dec si
dec lenstr_si
jmp to_start_si_bm_loop

end_bm:
inc si
push si

push bx
ret
bezznak_minus endp


;------------------------ЗНАКОВОЕ СЛОЖЕНИЕ---------------------
znak_plus proc
pop bx

mov ret_adress_n, bx

pop si   ;
pop di   ; Адреса начала чисел

pop dx ;; Старый bx из другой процедуры

push bx

push si
call skip_zeros
pop si

push di
call skip_zeros
pop di

pop bx

push dx

mov dh, [si]
mov dl, [di]

push di  ; Сохранил адреса начала чисел
push si  ;

push bx

push si
call to_end_str

push di
call to_end_str
pop di
pop ax   ; Длина строки di
pop si
pop cx   ; Длина строки si

pop bx

cmp dh, '-'
je jmp_si_has_minus_zp

jmp si_has_minus_zp_nj
jmp_si_has_minus_zp:
jmp si_has_minus_zp
si_has_minus_zp_nj:

cmp dl, '-'
je di_has_minus_zp

push bx               ;
push offset chislo1   ;
push offset chislo2   ;
call bezznak_plus     ; Когда у обоих нет минуса
pop si                ;
pop bx
push si
push bx
ret

di_has_minus_zp:
;;;Случай, когда минус только у DI

cmp cx, ax
jg si_is_bigger_zp_w
cmp cx, ax
jl di_is_bigger_zp_w

pop si
pop di

push di
push si

mov al, 0
mov [di], al
inc di

proverka_znplus_loop:

mov al, [si]
cmp al, [di]
jg si_is_bigger_zp

cmp al, [di]
jl di_is_bigger_zp

inc si
inc di
loop proverka_znplus_loop

mov dx, offset res_zero  ;;
mov ah, 09h              ;; 
int 21h                  ;; Числа равны по модулю
mov ah, 4ch              ;;
int 21h                  ;;

si_is_bigger_zp:
pop si
pop di

push bx

push di
push si

call bezznak_minus
pop si
pop bx
push si
push ret_adress_n
ret

di_is_bigger_zp:
pop si
pop di

mov res[0], '-'
push bx
push si
push di
call bezznak_minus
pop si
pop bx
push si
push ret_adress_n
ret
si_is_bigger_zp_w:
pop si
pop di

mov al, 0
mov [di], al

push bx

push di
push si

call bezznak_minus
pop si
pop bx ;
;pop bx ; На стеке остался мусор
;pop bx ;
push si
push ret_adress_n
ret

di_is_bigger_zp_w:
pop si
pop di

mov al, 0
mov [di], al
mov res[0], '-'
push bx
push si
push di
call bezznak_minus
pop si
pop bx
;pop bx
;pop bx
push si
push ret_adress_n
ret


si_has_minus_zp:
cmp dl, '-'
je di_and_si_has_minus_zp
;;;Случай, когда минус только у SI

xor cx, ax
xor ax, cx
xor cx, ax

pop si
pop di

xor si, di
xor di, si
xor si, di

push di
push si

jmp di_has_minus_zp

di_and_si_has_minus_zp:

pop di
pop si

mov al, 0
mov [di], al
mov [si], al
mov res[0], '-'

push bx

push offset chislo2
push offset chislo1
call bezznak_plus

pop si
pop bx
push si
push ret_adress_n
ret
znak_plus endp
;-------------------------------------ЗНАКОВОЕ ВЫЧИТАНИЕ------------------------------
proc znak_minus
pop bx
mov ret_adress, bx

pop si   ;
pop di   ; Адреса начала чисел

mov al, [di]
cmp al, '-'
je neg_di_zm
jmp not_minus_zm

neg_di_zm:
mov al, 0
mov [di], al

push di
push si
call znak_plus
pop si
push si
push ret_adress
ret

not_minus_zm:
mov dx, di
mov al, [di]
inc di
sdvig_loop:
mov ah, [di]
cmp ah, '$'
je exit_sdvig
mov [di], al
mov al, ah
inc di
jmp sdvig_loop
exit_sdvig:
mov [di], al
mov al, '$'
mov [di+1], al

mov al, '-'
mov di, dx
mov [di], al


push di
push si
call znak_plus
pop si
push si
push ret_adress
ret

znak_minus endp

;----------------------------БЕЗЗНАКОВОЕ УМНОЖЕНИЕ----------------------------
proc bezznak_mul
pop bx
mov ret_adress, bx

pop si   ;
pop di   ; Адреса начала чисел

push di
push si

push si
call to_end_str

push di
call to_end_str
pop di
pop ax   ; Длина строки di
pop si
pop cx   ; Длина строки si

cmp cx, ax
jge cx_bigger_bmul

pop di
pop si

jmp swap_si_di

cx_bigger_bmul:

pop si
pop di

swap_si_di:

mov si_adress, si
mov di_adress, di

push di
call eq_zero
pop ax
cmp ax, 0
je zero_res
jmp need_mul
zero_res:
push di_adress
call print
mov ah, 4ch
int 21h
need_mul:

mov di, offset chislo_to_mul
push ds
pop es
mov cx, 24
rep movsb


bmul_loop:             ;Цикл умножения
push offset minus_odin
push di_adress
call bezznak_minus

pop di
push di
call eq_zero
pop ax
cmp ax, 0
je exit_znal_mul

push offset chislo_to_mul
push si_adress
call bezznak_plus
pop bx
jmp bmul_loop

exit_znal_mul:

push si_adress

push ret_adress
ret
bezznak_mul endp
;----------------------------------------------16-РИЧНОЕ СЛОЖЕНИЕ----------------------
proc bezznak_plus_hex
pop bx
mov ret_adress, bx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pop si   ;
pop di   ; Адреса начала чисел

push si
call to_end_str

push di
call to_end_str
pop di
pop ax   ; Длина строки di
pop si
pop cx   ; Длина строки si

cmp cx, ax
jge cx_big_bezzplus_hex

xor cx, ax
xor ax, cx
xor cx, ax

xor si, di
xor di, si
xor si, di

cx_big_bezzplus_hex:

mov lenstr_si, cx
mov lenstr_di, ax

; В si - длинная строка, в cx - ее длина
bezzplus_loop_hex:

mov al, [perenos]
add al, [si]
mov [si], al
mov perenos, 0

cmp lenstr_di, 0
je end_di_bezzplus_hex

mov al, [di]
add al, [si]

cmp al, 15
jle not_perenos_bezznplus_hex
sub al, 16
mov perenos, 1
not_perenos_bezznplus_hex:
mov [si], al

dec si
dec di
dec lenstr_si
dec lenstr_di

loop bezzplus_loop_hex
jmp norm_ex_bp_hex
end_di_bezzplus_hex:

mov al, [perenos]
add al, [si]
mov [si], al
mov perenos, 0

mov al, [si]
cmp al, 15
jle not_perenos_bezzplus_end_hex
sub al, 16
mov perenos, 1
not_perenos_bezzplus_end_hex:
mov [si], al
dec si
dec lenstr_si
loop bezzplus_loop_hex
norm_ex_bp_hex:

mov al, perenos
cmp al, 1
je sdvig_si_hex
jmp not_sdvig_si_hex
sdvig_si_hex:
mov perenos, 0
inc si
mov dx, si
mov al, [si]
inc si
sdvig_loop_plus_hex:
mov ah, [si]
cmp ah, '$'
je exit_sdvig_plus_hex
mov [si], al
mov al, ah
inc si
jmp sdvig_loop_plus_hex
exit_sdvig_plus_hex:
mov [si], al
mov al, '$'
mov [si+1], al

mov si, dx
mov al, 1
mov [si], al

push si
push ret_adress
ret

not_sdvig_si_hex:
inc si
push si
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
push ret_adress
ret
bezznak_plus_hex endp

;------------------------------------------
proc znak_mul
pop bx
mov ret_adress_n, bx

pop si   ;
pop di   ; Адреса начала чисел

mov dl, [di]
mov dh, [si]

cmp dl, '-'
je di_minus_zm

cmp dh, '-'
je si_has_minus_zm
push si
push di
call bezznak_mul
push ret_adress_n
ret
	si_has_minus_zm:
mov al, 0
mov [si], al
mov res[0], '-'

push si
push di
call bezznak_mul
push ret_adress_n
ret

	di_minus_zm:
cmp dh, '-'
je di_and_si_minus_zm
;;;МИНУС ТОЛЬКО У DI

mov al, 0
mov [di], al
mov res[0], '-'

push si
push di
call bezznak_mul

push ret_adress_n
ret

di_and_si_minus_zm:
mov al, 0
mov [si], al
mov [di], al

push si
push di
call bezznak_mul

push ret_adress_n
ret
znak_mul endp
;---------------------ПРОХОД ДО КОНЦА СТРОКИ------------Кладет на стек адрес последнего элемента и длину строки
proc to_end_str
pop bx

pop si

mov was_not_zero, 0

xor cx, cx
to_end_loop:
mov al, [si]
cmp al, '$'
je exit_end_loop
cmp al, '-'
je not_len

cmp al, 0
jne was_nz
jmp wasnt_nz
was_nz:
mov was_not_zero, 1
wasnt_nz:

cmp was_not_zero, 0
je not_len

inc cx
not_len:
inc si

jmp to_end_loop

exit_end_loop:
dec si

cmp cx, 0
je inc_cx
jmp not_inc_cx
inc_cx:
inc cx
not_inc_cx:

push cx
push si

push bx
ret
to_end_str endp
;--------------------------------------Пропуск нулей в начале----------------
proc skip_zeros
pop bx
pop dx
push si
push di

mov si, dx

skip_zeros_loop:
mov al, [si]
cmp al, 0
jne exit_skip_zeros
inc si
jmp skip_zeros_loop
exit_skip_zeros:

cmp al, '$'
je inc_dollar
jmp exit_skip
inc_dollar:
dec si
exit_skip:
mov dx, si

pop di
pop si

push dx

push bx
ret
skip_zeros endp

;------------------------------------Проверка на равенство нулю---------------------
proc eq_zero
pop bx

pop di

eq_zero_loop:
mov al, [di]
cmp al, '$'
je exit_eq_zero

cmp al, 0
jne not_eq_zero
inc di
jmp eq_zero_loop
exit_eq_zero:
push 0
push bx
ret

not_eq_zero:
push 1
push bx
ret

eq_zero endp
code ends
end begin
