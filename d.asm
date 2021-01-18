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
	mov ax, 0x0720  ; space in normal attribute  
	mov cx, 2000 
	
	cld 
	rep stosw 

	pop di
	pop cx
	pop ax
	pop es
	ret

;//////////m1 printing ////////////  ; table 1 printing 
TAB1:
	push bp
	mov bp,sp
	
	mov ax,0xb800
	mov es,ax
	
	mov di,894
	mov ax,0x7131 ; print 1 in highlighted form over the segment first letter 
	mov [es:di],ax
	
	add di,20 

	mov si,table1 ; addres of string 0-7 
	mov cx,8
	mov ah, 0x07 ; normal attribute fixed in al
	
nextarr:
	mov al, [si] ; load next char of string
	mov [es:di], ax ; show this char on screen
	add di, 6 ; move to next screen location
	add si, 1 ; move to next char in string
	loop nextarr ; repeat the operation cx times
	
	mov si,[bp+4]   ; address from where to be printed 
	
	push si 
	
	;extra
	mov di,1054
	mov cx,10

	; printing  XS:XXXX 10 times 
DSSS:
	push cx
	push di
	
	mov ax,[bp+6] ; first character of the segment to be printed
	mov [es:di],ax
	
	add di,2
	mov ax,0x0753  ; S
	mov [es:di],ax
	
	add di,2
	mov ax,0x073A ; :
	mov [es:di],ax
	
	add di,2
	mov ax,si    ; mov starting address in ax which was stored in si 
	
	;dividing code 
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
	mov dh, 0x02 ; green on black background 
	mov [es:di], dx ; print char on screen
	
	add di,2
	loop nextp ; repeat for all digits on stack
	
	pop di
	add di,160
	add si,0x08
	pop cx
	loop DSSS
	
	
	pop si 
	
	;extra
	mov cx,10   ; now to print values in memory 
	mov di,1072    ; point at XS:0000

; 4 set of values printing 
m11:
	push cx
	mov cx,4
	push di

; one by one line from 0 - 7 
m1:
	mov ax,word[ds:si] 
	xchg al,ah   ; in order to cater little and big endian 
	push ax   ; pass what to print 
	call printable1;

	;inner loop for a row printing 
	add si,0x0002
	add di,2
	loop m1
	
	;outer loop for the 10 rows printing  
	pop di
	add di,160
	pop cx
	loop m11
	
	pop bp
	ret 4

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
	
	mov ax, [bp+4] ; load number in ax which is to be printed 
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
	mov dh, 0x02 ; green on black bg
	mov [es:di], dx ; print char on screen
	
	cmp cx,3 ; next location 
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

	
;////////////m2 printing function//////////// ; table 2 printing 
TAB2:
    push bp
	mov bp,sp
	
	mov ax,0xb800
	mov es,ax
	
	mov di,2560
	mov ax,0x7132 ; print 2 in bold 	
	mov [es:di],ax
	
	add di,22 

	mov si,table2 ; string 2 address 
	mov cx,16
	mov ah, 0x07 ; normal attribute fixed in al

nextcharr:
	mov al, [si] ; load next char of string
	mov [es:di], ax ; show this char on screen
	add di, 6 ; move to next screen location
	add si, 1 ; move to next char in string
	loop nextcharr ; repeat the operation cx times

	mov si,[bp+4]   ; starting addres from where to print 
	
	push si 
	
	;extra
	mov di,2720
	mov cx,5

DSS:
	push cx
	push di
	
	mov ax,[bp+6]  ; first character of the segment 
	mov [es:di],ax
	
	add di,2
	mov ax,0x0753 ; char S 
	mov [es:di],ax
	
	add di,2
	mov ax,0x073A  ; : 
	mov [es:di],ax
	add di,2
	
	mov ax,si ; ax now has the addres from where to print the table which was in si 

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
	mov dh, 0x02 ; green on black bg  attribute
	mov [es:di], dx ; print char on screen
	add di,2
	loop nextposs ; repeat for all digits on stack
	
	pop di
	add di,160
	add si,0x10
	pop cx
	loop DSS
	
	
	pop si 
	
	;extra
	mov cx,5
	mov di,2740

; 8 set of values printing 
m22:
	push cx
	mov cx,8
	push di

m2:
	mov ax,word[ds:si]
	
	xchg al,ah
	push ax
	call printable2;

	;inner loop for a row priting 
	add si,0x0002
	add di,2
	loop m2
	
	;outer loop for 5 rows printing 
	pop di
	add di,160
	pop cx
	loop m22
	
	pop bp
	ret 4

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





;////////////////////saving register values in memory ///////////////////////////

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
	
	;mov ax,[bp+18]
	;mov bx,[bp+16]
	;mov cx,[bp+14]
	;mov dx,[bp+12]
	
	
	mov word[stackzero],ax
	mov word[stacktwo],bx
	mov word[stackfour],cx
	mov word[stacksix],dx
	
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

	xor di,di 
	
	mov ax,0xb800
	mov es,ax
	
	mov cx,4
	mov ax,0x0741  ; A 
	push di

l1:       ; print four registres a,b,c,dx
	mov [es:di],ax 
	push ax
	add di,2
	mov ax,0x0758 ; X 
	mov [es:di],ax
	pop ax
	
	add di,158
	add ax,1  ; B ,  C , D 
	loop l1
	
	pop di

	mov cx,2
	add di,18
	mov ax,0x0753 ; S
	push di

l2:       ;print si and di
	mov [es:di],ax
	push ax
	
	add di,2
	mov ax,0x0749  ; I 
	mov [es:di],ax
	
	pop ax
	add di,158
	mov ax,0x0744 ; D
	loop l2

	; BP and SP 
	mov cx,2
	mov ax,0x0742   ; B

l3:     ;print bp and sp
	mov [es:di],ax
	push ax
	add di,2
	mov ax,0x0750   ; P
	mov [es:di],ax
	pop ax
	add di,158
	mov ax,0x0753 ; S 
	loop l3
	
	
	pop di
	mov cx,3
	add di,18
	mov ax,0x0743  ; C
	push di

l4:       ;print si and di
	mov [es:di],ax
	push ax
	add di,2
	mov ax,0x0753  ; S
	mov [es:di],ax
	pop ax
	add di,158
	add ax,0x01  ; D , E 
	loop l4

	; SS
	mov ax,0x0753   ; S
	mov [es:di],ax
	add di,2
	mov [es:di],ax
	pop di

	; IP 
	add di,18
	mov ax,0x0749   ; I
	mov [es:di],ax
	add di,2
	mov ax,0x0750 ; P
	mov [es:di],ax
	
	; HS and FS 
	add di,318
	mov ax,0x0748  ; H
	mov [es:di],ax
	add di,2
	mov ax,0x0753   ; S
	mov [es:di],ax
	
	push di 
	add di, 4 
	mov ax, 0x0231
	mov [es:di],ax
	add di,2 
	mov ax, 0x0239
	mov [es:di],ax
	add di,2
	mov ax, 0x0246
	mov [es:di],ax
	add di,2
	mov ax, 0x0235
	mov [es:di],ax
	add di,2
	pop di 
	
	add di,158

	mov ax,0x0746   ; F
	mov [es:di],ax
	add di,2 
	mov ax,0x0753  ; S
	mov [es:di],ax
	
	push di 
	add di,4 
	mov ax, 0x0231
	mov [es:di],ax
	add di,2 
	mov ax, 0x0239
	mov [es:di],ax
	add di,2
	mov ax, 0x0246
	mov [es:di],ax
	add di,2
	mov ax, 0x0235
	mov [es:di],ax
	add di,2
	pop di 
	
	
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
	mov bx, 16 ; use base 16 for division
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
	mov ax,0x022B  ; +

l50: ; print+ 
	mov [es:di],ax
	add di,160
	loop l50
	
	; spaces 
	mov di,86
	mov cx,4 
	mov ax,0x0230 ; char 0 

; 0,2,4,6
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
	mov ax,[stacksix]
	push ax
	call pr

	mov di,116
	mov ax,word[val_FLAGS]
	push ax 
	call pr

	mov di,60
	mov ax,word[val_IP]
	push ax 
	call pr
	
	mov di,42
	mov ax,word[val_CS]
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
	
	mov cx, [bp+4]   ; lenght of stirng 
	mov si, [bp+6] ; address of string 
	mov ax, [bp+8] ; attribute of the string 

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
	mov di,74     ; at this position it print Stack 
	mov ax,0x0700 ; 
	push ax
	mov ax,str1
	push ax
	mov ax,[len1]
	push ax
	call printstr

	mov di,104     ; at this position it print Flags
	mov ax,0x0700 ; white on black bg 
	push ax
	mov ax,strflg
	push ax
	mov ax,[lenflg]
	push ax
	call printstr

	mov di,3680
	mov ax,0xb800
	mov es,ax
	
	;last line strings 
	mov ax, 0x0731   
	mov [es:di],ax
	add di,2

	mov ax,0x7000   ; black on white bg 
	push ax
	mov ax,str2
	push ax
	mov ax,[len2]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0732 ; 2
	mov [es:di],ax
	add di,2

	mov ax,0x7000 ;; black on white bg
	push ax
	mov ax,str3
	push ax
	mov ax,[len3]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0733 ; 3 
	mov [es:di],ax
	add di,2

	mov ax,0x7000  ; black on white bg
	push ax
	mov ax,str4
	push ax
	mov ax,[len4]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0734  ; 4 
	mov [es:di],ax
	add di,2

	mov ax,0x7000 ; black on white bg
	push ax
	mov ax,str5
	push ax
	mov ax,[len5]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0735 ; 5 
	mov [es:di],ax
	add di,2

	mov ax,0x7000 ; black on white bg
	push ax
	mov ax,str6
	push ax
	mov ax,[len6]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0736 ; 6 
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
	mov ax, 0x0737   ; 7 
	mov [es:di],ax
	add di,2

	mov ax,0x7000  ; black on white bg
	push ax
	mov ax,str7
	push ax
	mov ax,[len7]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0738 ; 8 
	mov [es:di],ax
	add di,2

	mov ax,0x7000  ; black on white bg
	push ax
	mov ax,str8
	push ax
	mov ax,[len8]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0739 ; 9 
	mov [es:di],ax
	add di,2

	mov ax,0x7000   ; black on white bg
	push ax
	mov ax,str9
	push ax
	mov ax,[len9]
	push ax
	call printstr

	add di,4
	mov ax,0xb800
	mov es,ax
	mov ax, 0x0731  ; 1
	mov [es:di],ax
	add di,2
	mov ax,0x0730   ; 0 
	mov [es:di],ax
	add di,2

	mov ax,0x7000   ; black on white bg
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
	mov ax,0x075F ; ____

l8:
	mov [es:di],ax
	add di,2
	loop l8
	pop di
	
	add di,158
	mov ax,0x077C ; |
	mov [es:di],ax
	
	add di,160
	mov [es:di],ax
	add di,2
	mov cx,0
	mov ax,0x075F  ; _

l9:
	mov [es:di],ax
	add di,2
	add cx,2
	cmp cx,86   ; cx 86
	jne l9
	
	push di
	mov ax,0x077C ; |
	mov [es:di],ax
	
	sub di,160
	mov ax,0x077C ; |
	mov [es:di],ax
	pop di
	add di,158
	mov cx,6
	mov ax,0x077C ; |

l10:
	mov [es:di],ax
	add di,160
	loop l10
	
	mov cx,43
	mov ax,0x075F   ; _
	sub di,2

l11:
	mov [es:di],ax
	sub di,2
	loop l11
	
	add di,2
	mov cx,6
	mov ax,0x077C  ; |
	sub di,160

l12:
	mov [es:di],ax
	sub di,160
	loop l12

	mov cx,80
	mov di,3520
	mov ax,0x075F ; _

l13:  ;last lower border
	mov [es:di],ax
	add di,2
	loop l13

	mov cx,7
	sub di,32
	sub di,160
	mov ax,0x077C  ; |

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

  

blinkingC:
	push bp
	mov bp,sp
	
	pusha
    
	mov ah,0x13
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 7 ; normal attrib
	mov dx, 0x0502 ; row 6(5) column 3(2)
	mov cx,[cs:val] ; length of string
	push cs
	pop es ; segment of string ; cs=es
	mov bp, message ; offset of string
	int 0x10 ; call BIOS video service
	
	popa
	
	pop bp
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
	
	mov dx,word[bp+6]   ; flags
	mov word[val_FLAGS],dx	   ; flags 
	
	mov dx,word[bp+4]  ; cs
	mov word[val_CS],dx		; cs 
	
	mov dx,word[bp+2]      ; ip 
	mov word[val_IP],dx ; ip
   
	pop dx  ; restore originla dx 
	
	call savevaluesofreg
	call clrscr
    call printregistervalues
	call printborders
	call blinkingC
	
	push 0x0744   ; char  D
	push 0x0000   ; starting address 
    call TAB1   ; m1
	
	push 0x0744    ; char D 
 	push 0x0000    ; starting address 
	call TAB2    ; m2
	
	call printregisters
	call printstrings
	
waittt:
	mov ah, 0 ; service 0 – get keystroke
	int 0x16 ; call BIOS keyboard service
	cmp ah,0x3B   ; F1's scancode  scan code is in ah  
	je endss
	inc word[cs:val]
	call blinkingC
	je endss
	jmp waittt
	
	
endss:
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
	mov es, ax ; point es to IVT base ; 0 

	; hooking 
	mov word [es:1*4], trapisr ; store offset at n*4
	mov [es:1*4+2], cs ; store segment at n*4+2

	;turn on the trap flag 
	; chaining 
	
	pushf ; save flags on stack
	pop ax ; copy flags in ax
	or ax, 0x0100 ; set bit corresponding to TF ; 8th bit from left 
	push ax ; save ax on stack
	popf ; reload into flags register

	; the trap flag bit is on now, INT 1 will come after next instruction
	; sample code to check the working of our elementary debugger
	; infinite loop 
	
	mov ax, 0
	mov bx, 0x10
	mov cx, 0x20
	mov dx, 0x40

	; changes  
	; ax , bx , cx , dx , si , di 

infinite_loop_lTrap:
    inc ax
	add bx, 2
	dec cx
	sub dx, 2
	mov si,0x200
	mov di,si
	jmp infinite_loop_lTrap

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
stacksix: dw 0

val_FLAGS: dw 0
val_IP: dw 0
val_CS: dw 0

;strings
str1: db 'Stack'
len1: dw 5

str2: db 'Step'
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

strflg: db 'Flags'
lenflg: dw 5

;////// for table strings//////////
table2: db '0123456789ABCDEF'  
table1: db '01234567'

; print CMD 
val : dw 7
message: db 'CMD > '