[org 0x0100]
jmp start


;////////clear screen function/////////////////
clrscr:
push es
push ax
push cx
push di

mov ax, 0xb800
mov es, ax 
xor di, di 
mov ax, 0x0720 
mov cx, 2000 
cld 
rep stosw 

pop di
pop cx
pop ax
pop es
ret
;//////////m1 printing ////////////
TAB1:
mov ax,0xb800
mov es,ax
mov di,904
mov ax,0x7131
mov [es:di],ax
add di,10

mov si,table1
mov cx,8
mov ah, 0x07 ; normal attribute fixed in al
nextarr:
 mov al, [si] ; load next char of string
mov [es:di], ax ; show this char on screen
add di, 6 ; move to next screen location
add si, 1 ; move to next char in string
loop nextarr ; repeat the operation cx times





mov si,0x0000
;extra
mov di,1054
mov cx,10
DSSS:
push cx
push di
mov ax,0x0744
mov [es:di],ax
add di,2
mov ax,0x0753
mov [es:di],ax
add di,2
mov ax,0x073A
mov [es:di],ax
add di,2
mov ax,si

;copy
mov bx, 16 ; use base 10 for division
mov cx, 0 ; initialize count of digits

ndigit: 
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
cmp dx,9
ja looopa
add dl, 0x30 ; convert digit into ascii value
jmp ju
looopa:
add dl,0x37
jmp ju
ju:
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp cx, 4 ; is the quotient zero
jne ndigit  ; if no divide it again
;;mov di, 0 ; point di to top left column


nextp:
pop dx ; remove a digit from the stack
mov dh, 0x02 ; use normal attribute
mov [es:di], dx ; print char on screen
add di,2
loop nextp ; repeat for all digits on stack
pop di
add di,160
add si,0x08
pop cx
loop DSSS




;extra

mov cx,10
mov di,1072


m11:
push cx
mov cx,4
push di
m1:
mov ax,word[ds:si]
xchg al,ah
push ax
call printable1;

add si,0x0002
add di,2
loop m1
pop di
add di,160
pop cx
loop m11
ret

printable1:
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx

mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 16 ; use base 10 for division
mov cx, 0 ; initialize count of digits

nit: 
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
cmp dx,9
ja la
add dl, 0x30 ; convert digit into ascii value
jmp jpp
la:
add dl,0x37
jmp jpp
jpp:
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp cx, 4 ; is the quotient zero
jne nit  ; if no divide it again
;;mov di, 0 ; point di to top left column


nexps:
pop dx ; remove a digit from the stack
mov dh, 0x02 ; use normal attribute
mov [es:di], dx ; print char on screen
cmp cx,3
je spa
add di,2
jmp ll1
spa:
add di, 4 ; move to next screen location
ll1:
loop nexps ; repeat for all digits on stack



pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2  



;////////////m2 printing function////////////
TAB2:
mov ax,0xb800
mov es,ax
mov di,2560
mov ax,0x7132
mov [es:di],ax
add di,20

mov si,table2
mov cx,16
mov ah, 0x07 ; normal attribute fixed in al
nextcharr:
 mov al, [si] ; load next char of string
mov [es:di], ax ; show this char on screen
add di, 6 ; move to next screen location
add si, 1 ; move to next char in string
loop nextcharr ; repeat the operation cx times





mov si,0x0000
;extra
mov di,2720
mov cx,5
DSS:
push cx
push di
mov ax,0x0744
mov [es:di],ax
add di,2
mov ax,0x0753
mov [es:di],ax
add di,2
mov ax,0x073A
mov [es:di],ax
add di,2
mov ax,si

;copy
mov bx, 16 ; use base 10 for division
mov cx, 0 ; initialize count of digits

nextdigittt: 
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
cmp dx,9
ja lopa
add dl, 0x30 ; convert digit into ascii value
jmp jumppp
lopa:
add dl,0x37
jmp jumppp
jumppp:
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp cx, 4 ; is the quotient zero
jne nextdigittt  ; if no divide it again
;;mov di, 0 ; point di to top left column


nextposs:
pop dx ; remove a digit from the stack
mov dh, 0x02 ; use normal attribute
mov [es:di], dx ; print char on screen
add di,2
loop nextposs ; repeat for all digits on stack
pop di
add di,160
add si,0x10
pop cx
loop DSS




;extra

mov cx,5
mov di,2740


m22:
push cx
mov cx,8
push di
m2:
mov ax,word[ds:si]
xchg al,ah
push ax
call printable2;

add si,0x0002
add di,2
loop m2
pop di
add di,160
pop cx
loop m22
ret

printable2:
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx

mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 16 ; use base 10 for division
mov cx, 0 ; initialize count of digits

nnextdigit: 
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
cmp dx,9
ja loopa
add dl, 0x30 ; convert digit into ascii value
jmp jumpp
loopa:
add dl,0x37
jmp jumpp
jumpp:
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp cx, 4 ; is the quotient zero
jne nnextdigit  ; if no divide it again
;;mov di, 0 ; point di to top left column


nexxtpos:
pop dx ; remove a digit from the stack
mov dh, 0x02 ; use normal attribute
mov [es:di], dx ; print char on screen
cmp cx,3
je space
add di,2
jmp l30
space:
add di, 4 ; move to next screen location
l30:
loop nexxtpos ; repeat for all digits on stack



pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2  
;///////////here m2 printing function ends

;////////////////////reguster values///////////////////////////
savevaluesofreg:
push bp
mov bp,sp


mov word[valofax],ax
mov word[valofbx],bx
mov word[valofcx],cx
mov word[valofdx],dx

mov ax,[bp+4]
mov bx,[bp+6]
mov cx,[bp+8]
mov dx,[bp+10]

mov word[stackzero],ax
mov word[stacktwo],bx
mov word[stackfour],cx
mov word[stackeight],dx

mov word[valofsi],si
mov word[valofdi],di
mov word[valofbp],bp
mov word[valofsp],sp


mov word[valofcs],cs
mov word[valofds],ds
mov word[valofes],es
mov word[valofss],ss
pop bp
ret



;just print ax.bx.cx
printregisters:
push ax
push bx
push cx
push dx
push di


mov di,0
mov ax,0xb800
mov es,ax
mov cx,4
mov ax,0x0741
push di
l1:       ; print four registres a,b,c,dx
mov [es:di],ax
push ax
add di,2
mov ax,0x0758
mov [es:di],ax
pop ax
add di,158
add ax,1
loop l1
pop di

mov cx,2
add di,18
mov ax,0x0753
push di
l2:       ;print si and di
mov [es:di],ax
push ax
add di,2
mov ax,0x0749
mov [es:di],ax
pop ax
add di,158
mov ax,0x0744
loop l2


mov cx,2
mov ax,0x0742
l3:     ;print bp and sp
mov [es:di],ax
push ax
add di,2
mov ax,0x0750
mov [es:di],ax
pop ax
add di,158
mov ax,0x0753
loop l3
pop di



mov cx,3
add di,18
mov ax,0x0743
push di
l4:       ;print si and di
mov [es:di],ax
push ax
add di,2
mov ax,0x0753
mov [es:di],ax
pop ax
add di,158
add ax,0x01
loop l4

mov ax,0x0753
mov [es:di],ax
add di,2
mov [es:di],ax
pop di


add di,18
mov ax,0x0749
mov [es:di],ax
add di,2
mov ax,0x0750
mov [es:di],ax
add di,318

mov ax,0x0748
mov [es:di],ax
add di,2
mov ax,0x0753
mov [es:di],ax
add di,158

mov ax,0x0746
mov [es:di],ax
add di,2
mov ax,0x0753
mov [es:di],ax
add di,158



pop di
pop dx
pop cx
pop bx
pop ax
ret






;;to print values of registers
pr:
push bp
mov bp,sp
push ax
push bx
push cx
push dx
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax,0
mov ax, [bp+4] ; load number in ax
cmp ax,0
mov bx, 16 ; use base 10 for division
mov cx, 0 ; initialize count of digits

n: 
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
cmp dx,9
ja laaa
add dl, 0x30 ; convert digit into ascii value
jmp jppp
laaa:
add dl,0x37
jmp jppp
jppp:
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp cx, 4 ; is the quotient zero
jne n  ; if no divide it again
;;mov di, 0 ; point di to top left column
jmp nexxps


nexxps:
pop dx ; remove a digit from the stack
mov dh, 0x02 ; use normal attribute
mov [es:di], dx ; print char on screen
add di,2
loop nexxps ; repeat for all digits on stack
pop dx
pop cx
pop bx
pop ax
pop bp
ret 2










printregistervalues:   ; 
push ax
push bx
push cx
push dx

mov di,6
mov ax,word[valofax]
push ax
call pr
add di,152
mov ax,word[valofbx]
push ax
call pr
add di,152
mov ax,word[valofcx]
push ax
call pr
add di,152
mov ax,[valofdx]
push ax
call pr



mov di,24
mov ax,word[valofsi]
push ax
call pr
add di,152
mov ax,word[valofdi]
push ax
call pr
add di,152
mov ax,word[valofbp]
push ax
call pr
add di,152
mov ax,[valofsp]
push ax
call pr


mov di,42
mov ax,word[valofcs]
push ax
call pr
add di,152
mov ax,word[valofds]
push ax
call pr
add di,152
mov ax,word[valofes]
push ax
call pr
add di,152
mov ax,[valofss]
push ax
call pr

mov di,84

mov cx,4
mov ax,0x022B
l50:
mov [es:di],ax
add di,160
loop l50

mov di,86
mov cx,4
mov ax,0x0230
l51:
mov [es:di],ax
add al,0x02
add di,160
loop l51


mov di,90
mov ax,word[stackzero]
push ax
call pr
add di,152
mov ax,word[stacktwo]
push ax
call pr
add di,152
mov ax,word[stackfour]
push ax
call pr
add di,152
mov ax,[stackeight]
push ax
call pr


		mov di,116
		mov ax,word[spare1]
		push ax 
		call pr

		mov di,60
		mov ax,word[spare2]
		push ax 
		call pr
        
		mov di,42
		mov ax,word[spare3]
		push ax 
		call pr


pop dx
pop cx
pop bx
pop ax
ret


printstr:
push bp
mov bp, sp
push es
push ax
push cx
push si
mov ax, 0xb800
mov es, ax
mov si, [bp+6] 
mov cx, [bp+4] 
mov ax, [bp+8] 

nextchar:
mov al, [si] 
mov [es:di], ax 
add di, 2 
add si, 1 
loop nextchar 
pop si
pop cx
pop ax
pop es
pop bp
ret 6


printstrings:


mov di,74     ; at this position it print stack 
mov ax,0x0700
push ax
mov ax,str1
push ax
mov ax,[len1]
push ax
call printstr




mov di,104     ; at this position it print stack 
mov ax,0x0700
push ax
mov ax,strflg
push ax
mov ax,[lenflg]
push ax
call printstr



mov di,3680
mov ax,0xb800
mov es,ax
mov ax, 0x0731
mov [es:di],ax
add di,2

mov ax,0x7000
push ax
mov ax,str2
push ax
mov ax,[len2]
push ax
call printstr

add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0732
mov [es:di],ax
add di,2


mov ax,0x7000
push ax
mov ax,str3
push ax
mov ax,[len3]
push ax
call printstr

add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0733
mov [es:di],ax
add di,2

mov ax,0x7000
push ax
mov ax,str4
push ax
mov ax,[len4]
push ax
call printstr

add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0734
mov [es:di],ax
add di,2

mov ax,0x7000
push ax
mov ax,str5
push ax
mov ax,[len5]
push ax
call printstr

add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0735
mov [es:di],ax
add di,2

mov ax,0x7000
push ax
mov ax,str6
push ax
mov ax,[len6]
push ax
call printstr

add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0736
mov [es:di],ax
add di,2



mov ax,0x7020
mov cx,5
l7:
mov [es:di],ax
add di,2
loop l7


add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0737
mov [es:di],ax
add di,2


mov ax,0x7000
push ax
mov ax,str7
push ax
mov ax,[len7]
push ax
call printstr



add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0738
mov [es:di],ax
add di,2

mov ax,0x7000
push ax
mov ax,str8
push ax
mov ax,[len8]
push ax
call printstr


add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0739
mov [es:di],ax
add di,2


mov ax,0x7000
push ax
mov ax,str9
push ax
mov ax,[len9]
push ax
call printstr


add di,4
mov ax,0xb800
mov es,ax
mov ax, 0x0731
mov [es:di],ax
add di,2
mov ax,0x0730
mov [es:di],ax
add di,2



mov ax,0x7000
push ax
mov ax,str10
push ax
mov ax,[len10]
push ax
call printstr
ret



printborders:   ; lines in the afd 
push bp
mov bp, sp
push es
push ax
push cx
push si
mov ax, 0xb800
mov es, ax

mov di,642
push di
mov cx,79
mov ax,0x075F
l8:
mov [es:di],ax
add di,2
loop l8

mov di,806
mov ax,0x0743
mov [es:di],ax
add di,2

mov ax,0x074D
mov [es:di],ax
add di,2

mov ax,0x0744
mov [es:di],ax
add di,2
mov ax,0x0720
mov [es:di],ax
add di,2

mov ax,0x073E
mov [es:di],ax
add di,2


mov ax,0x875F
mov [es:di],ax
pop di



add di,158
mov ax,0x077C
mov [es:di],ax
add di,160
mov [es:di],ax
add di,2
mov cx,0
mov ax,0x075F
l9:
mov [es:di],ax
add di,2
add cx,2
cmp cx,86   ; cx 86
jne l9
push di
mov ax,0x077C
mov [es:di],ax
sub di,160
mov ax,0x077C
mov [es:di],ax

pop di


add di,158
mov cx,6
mov ax,0x077C
l10:
mov [es:di],ax
add di,160
loop l10

mov cx,43
mov ax,0x075F
sub di,2
l11:

mov [es:di],ax
sub di,2
loop l11
add di,2




mov cx,6
mov ax,0x077C
sub di,160
l12:
mov [es:di],ax
sub di,160
loop l12

mov cx,80
mov di,3520
mov ax,0x075F
l13:      ;llast lower border
mov [es:di],ax
add di,2
loop l13


mov cx,7
sub di,32
sub di,160
mov ax,0x077C
l14:
mov [es:di],ax
sub di,160
loop l14




pop si
pop cx
pop ax
pop es
pop bp
ret 


;printnumfunction/////////////////////////////////////////

printnum:
 push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 16 ; use base 10 for division
mov cx, 0 ; initialize count of digits

nextdigit:
 mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
cmp dx,9
ja loopaa
add dl, 0x30 ; convert digit into ascii value
jmp jummpp
loopaa:
add dl,0x37
jummpp:
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again

nextpos:
 pop dx ; remove a digit from the stack
mov dh, 0x02 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2  


blinkingC:

	mov al, 1 ; subservice 01 – update cursor
mov bh, 0 ; output on page 0
mov bl, 7 ; normal attrib
mov dx, 0x0501 ; row 10 column 3
mov cx, 11 ; length of string
push cs
pop es ; segment of string
mov bp, message ; offset of string
int 0x10 ; call BIOS video service
ret 


trapisr:
        push bp
		mov bp, sp ; to read cs, ip and flags
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		push ds
		push es


		
		push dx
		mov dx,word[bp+6]
		mov word[spare1],dx
		
		mov dx,word[bp+4]
		mov word[spare3],dx
		
		
		mov dx,word[bp+2]
		mov word[spare2],dx
      
	  pop dx
		call savevaluesofreg
call clrscr
;call blinkingC
call printregistervalues

call printborders
call TAB1
call TAB2
call printregisters
call printstrings


mov ah, 0 ; service 0 – get keystroke
int 0x16 ; call BIOS keyboard service
		
		pop es
		pop ds
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax 
		pop bp
	iret


start:
	xor ax, ax
	mov es, ax ; point es to IVT base

	mov word [es:1*4], trapisr ; store offset at n*4
	mov [es:1*4+2], cs ; store segment at n*4+2

	pushf ; save flags on stack
	pop ax ; copy flags in ax
	or ax, 0x100 ; set bit corresponding to TF
	push ax ; save ax on stack
	popf ; reload into flags register

; the trap flag bit is on now, INT 1 will come after next instruction
; sample code to check the working of our elementary debugger
	mov ax, 0
	mov bx, 0x10
	mov cx, 0x20
	mov dx, 0x40
ltrap: 	
    inc ax
	add bx, 2
	dec cx
	sub dx, 2
	jmp ltrap



mov ax,0x4c00
int 21h


;local variables

valofax: dw 0
valofbx: dw 0
valofcx: dw 0
valofdx: dw 0

valofsi: dw 0
valofdi: dw 0
valofbp: dw 0
valofsp: dw 0


valofcs: dw 0
valofds: dw 0
valofes: dw 0
valofss: dw 0



stackzero: dw 0
stacktwo: dw 0
stackfour: dw 0
stackeight: dw 0


spare1: dw 0
spare2: dw 0
spare3: dw 0
;strings
str1: db 'Stack'
len1: dw 5

str2: db 'step'
len2: dw 4

str3: db 'ProcStep'
len3: dw 8

str4: db 'Retrieve'
len4: dw 8

str5: db 'Help ON'
len5: dw 7

str6: db 'BRK Menu'
len6: dw 8

str7: db 'up'
len7: dw 2

str8: db 'dn'
len8: dw 2

str9: db 'le'
len9: dw 2

str10: db 'ri'
len10: dw 2

message: db 'CMD > '

strflg: db 'flags'
lenflg: dw 5
;////// for table strings//////////
table2: db '0123456789ABCDEF'

table1: db '01234567'
