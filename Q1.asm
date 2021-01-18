[org 0x0100]

jmp start

compare:
push bp
mov bp, sp

push cx
push si
push di
push ds
push es

mov cx, -1
push ds
pop es

mov si, [bp + 6] ;string 1
mov di, [bp + 4] ;string 2
dec si
dec di

l1:
inc cl
add si, 1
cmp byte [ds:si], 0
jne l1

l2:
inc ch
add di, 1
cmp byte [es:di], 0
jne l2


cmp cl, ch
ja skp
mov cl, ch

skp:
mov ch, 0

std
repe cmpsb

cmp cx, 0
je set
mov dx, 0
jmp end


set:
mov dx, 1

end:
pop es
pop ds
pop di
pop si
pop cx
pop bp
ret 4


takestr:

push ax
push cx
push si

mov si, string1
mov cx, 50

nextchar: 
mov ah, 1 
int 0x21
cmp al, 13
je exit 
mov [si], al 
inc si 
loop nextchar

exit:
mov byte [si], 0
mov si, string2

nextchar1: 
mov ah, 1 
int 0x21
cmp al, 13
je exit1 
mov [si], al 
inc si 
loop nextchar1

exit1:
mov byte [si], 0
pop si
pop cx
pop ax
ret

start:

call takestr

push string1
push string2
call compare

mov ax, 0x4c00
int 0x21

string1: times 50 db 0
string2: times 50 db 0