.386
.model flat, stdcall

includelib msvcrt.lib
extern exit: proc
extern fprintf: proc
extern fscanf:proc
extern fclose:proc
extern printf: proc
extern scanf:proc
extern fopen:proc
public start

.data
matr1 DD 102 dup(?)
matr2 DD 102 dup(?)
endline DB 10, 13, 0
spatiu DB " ",0
operatii DB  "Introduceti o operatie pe matrici:",10,13,
				"> 1.Adunarea" ,10,13,
				"> 2.Diferenta",10,13,
				"> 3.Inmultirea cu un scalar",10,13,
				"> 4.Transpusa",10,13,
				"> 5.Determinantul",10,13,
				"> 6.Exit",10,13,0
format_matr1 DB "A=",0
format_matr2 DB "B=",0
file_name1 DB 10 dup(?),0
file_name2 DB 10 dup(?),0
file_name3 DB "rezultat.txt",0
lung_1 DD ?
lung_2 DD ?
lungime_matrice DD ?
int_format DB "%d",0
mr DB "r",0
mw DB "w",0
sir DB "%s",0
sum DD ?
file_a DD 0
file_b DD 0
file_r DD 0
element DD ?
element1 DD ?
mesaj1 DB "Matricea A a fost citita cu succes!",0
mesaj2 DB "Matricea B a fost citita cu succes!",0
tasta DD ?
scl DD 0
dif DD ?
result DD ?
msg DB "Dati un scalar", 0
aleg DB "Alegeti:",0
det_2 DD 0
det_1 DD 0
res_det DD ?
suma DD 0
dim DD 0
index DD ?
incorect DB "Operatie incorecta", 10,13,0
.code

;deschiere fisier
   open_file macro mode, file_name, file
	  push offset mode
	  push offset file_name
	  call fopen
	  add esp,8
	  mov file, eax
   endm
 
;citire n din fisier 
    citire_dim macro n,file
	   push offset n
	   push offset int_format
	   push file
	   call fscanf
	   add esp,12
	endm
;scriere in fisier
write_file macro numar, file
  
   push numar
   push offset int_format
   push file
   call fprintf
   add esp,12 
 endm
 
 ;pune spatiu in fisier intre elemente
pune_spatiu macro file
      push offset spatiu
	  push offset sir
	  push file
	  call fprintf
	  add esp,12 
endm
;rand nou in fisier 
pune_rand_nou macro file
    push offset endline
	push file
	call fprintf
	add esp,8
endm
;inchidere fisier
close_file macro file 
push file 
call fclose
add esp,4
endm

;adunarea  pentru a folosi la adunarea matricilor
adunare_2 macro v1,v2
 xor edx,edx
 mov edx,v1
 add edx,v2
 
endm

 ;adunarea matricilor
ADUNARE_MATRICI macro a,b,lung,n
local bucla,et1,et2
  open_file mw, file_name3, file_r
  xor esi,esi
  xor ebx,ebx
  mov ebx,n
 
  ;n=3
  ;lung=9
  
 
  bucla:
	  adunare_2 a[esi*4],b[esi*4]
	  mov sum,edx
	 write_file sum,file_r
	  pune_spatiu file_r
	 push sum
	 push offset int_format
	 call printf
	 add esp,8
	 push offset spatiu
	 push offset sir
	 call printf
	 add esp,8
	  inc esi
	  cmp esi, lung
	  je et2
	
	  dec ebx
	  cmp ebx,0
	  je et1
	  jmp bucla
et1: 
      pune_rand_nou file_r
      push offset endline
	  call printf
	  add esp,4
	  mov ebx,n
	  jmp bucla
et2: 
      pune_rand_nou file_r
	  push offset endline
	  call printf
	  add esp,4
  	  close_file file_r
endm	  
inmul proc
   push EBP
   mov EBP, ESP
   XOR EBX,EBX
   mov EBX, [EBP+8]
   imul EBX, [EBP+12]
   mov ESP,EBP
   pop EBP
   ret 8
inmul ENDP
;scaderea pentru a folosi la scaderea matricilor 
scadere_2 macro v1,v2
   xor edx,edx
   mov edx,v1
   sub edx,v2

  
endm

SCADERE_MATRICI macro a,b,lung,n
local bucla,et1,et2
  open_file mw, file_name3, file_r
  mov esi,0
  xor ebx,ebx
  mov ebx,n
  
 
  bucla:
	  scadere_2 a[esi*4],b[esi*4]
	  mov dif,edx
	  write_file dif,file_r
	  pune_spatiu file_r
	 push dif
	 push offset int_format
	 call printf
	 add esp,8
	 push offset spatiu
	 push offset sir
	 call printf
	 add esp,8
	  inc esi
	  cmp esi, lung
	  je et2
	  dec ebx
	  cmp ebx, 0
	  je et1
	  jmp bucla
et1:
      pune_rand_nou file_r
	  push offset endline
	 call printf
	 add esp,4
	  mov ebx,n
	  jmp bucla
et2: 
      pune_rand_nou file_r
   	  push offset endline
	  call printf
	  add esp,4
	  close_file file_r
endm	  

SCALAR_MATRICE macro a,scl,lung,n
local bucla,et1,et2
  open_file mw, file_name3, file_r
  mov esi,0
  mov ebp,n
  bucla:
	  push scl
	  push a[esi*4]
	  call inmul
	  mov result, ebx
	  push result
	  push offset int_format
	  call printf
	  add esp,8
	  push offset spatiu
	  push offset sir
	  call printf
	  add esp,8
	  write_file result,file_r
	  pune_spatiu file_r
	  inc esi
	  cmp esi, lung
	  je et2
	  dec ebp
	  cmp ebp, 0
	  je et1
	  jmp bucla
et1:
      pune_rand_nou file_r
	  push offset endline
	  call printf
	  add esp,4
	  mov ebp,n
	  jmp bucla
et2: 
      pune_rand_nou file_r
	  push offset endline
	  call printf
	  add esp,4
	  close_file file_r
	  
   	  
endm  
DETERMINANT_2 macro a,lung,n
    
	open_file mw, file_name3, file_r
     xor ebx,ebx
	 mov ebx,0
     mov esi,0
     push a[esi*4]
	 mov esi,lung
	 dec esi
	 push a[esi*4]
	 call inmul
	 mov det_2,ebx
	 xor esi,esi
	 xor ebx,ebx
	 mov ebx,0
	 mov esi,1
	 push a[esi*4]
	 inc esi
	 push a[esi*4]
	 call inmul
	 mov det_1,ebx
	 xor edx,edx
	 mov edx, det_2
	 sub edx, det_1
	 mov res_det,edx
	 write_file res_det,file_r
	 push res_det
	 push offset int_format
	 call printf
	 add esp,8
	 
	 push offset endline
     call printf
     add esp,4 
	 close_file file_r
	 
  endm 





TRANSPUSA_MATRICE macro a, lung, n
open_file mw, file_name3, file_r

xor esi,esi
push a[esi*8]
push offset int_format
call printf 
add esp,8
write_file a[esi*8], file_r
push offset spatiu
push offset sir
call printf
 add esp,8
 pune_spatiu file_r
inc esi
push a[esi*8]
push offset int_format
call printf 
add esp,8
write_file a[esi*8], file_r
push offset endline
call printf
add esp,4
pune_rand_nou file_r
mov esi,1
push a[esi*4]
push offset int_format
call printf
add esp,8
write_file a[esi*4], file_r
push offset spatiu
push offset sir
call printf
 add esp,8
 pune_spatiu file_r
 add esi,2
 push a[esi*4]
push offset int_format
call printf
add esp,8
write_file a[esi*4], file_r
push offset endline
call printf
add esp,4
pune_rand_nou file_r

 
close_file file_r
	  
endm

start:
   ;fisierul a.txt
    push offset format_matr1
	call printf
	add esp, 4
	
	push offset file_name1
	push offset sir
	call scanf
	add esp,8
	;deschidere fisier
	open_file mr,file_name1,file_a
	
	
	;fiserul b.txt
	push offset format_matr2
	call printf
	add esp, 4
	
	push offset file_name2
	push offset sir
	call scanf
	add esp,8
	;deschidere fisier
	open_file mr, file_name2,file_b
	

	
	;citim din ambele fisier n pentru a putea calcula cat ar veni lungimea unei matrici
	citire_dim lung_1, file_a
	 
	citire_dim lung_2 ,file_b
	 
	;calculam lungimea matricii in variabila lungime_matrice

	push lung_2
	push lung_1
	call inmul
	mov lungime_matrice, ebx
	
	
	
     
	push offset endline
	call printf
	add esp,4
	
	;citire fisier matrice A
	  mov esi,0
	  mov edi,0
	bucla_citire_A:
	    push offset element
		push offset int_format
		push file_a
		call fscanf
		add esp,12
		mov edi,element
		mov matr1[esi*4],edi
		inc esi
		cmp esi,lungime_matrice
		je spa
		jmp bucla_citire_A
    spa:
	   push offset mesaj1
	   call printf
	   add esp,4
    
	push offset endline
	call printf
	add esp,4 
	
    
	
		
    ;citire matrice B
	  xor esi,esi
	  xor  edi,edi
	bucla_citire_B:
	   
	    push offset element1
		push offset int_format
		push file_b
		call fscanf
		add esp,12
		mov edi,element1
		mov matr2[esi*4],edi
		inc esi
		cmp esi,lungime_matrice
		je stop
		jmp bucla_citire_B
    stop:
	   push offset mesaj2
	   call printf
	   add esp,4
	   
   
	push offset endline
	call printf
	add esp,4  	
	
start_bucla_meniu:
   
   push offset operatii
   call printf
   add esp, 4
   
   push offset endline
   call printf
   add esp,4 
   
   push offset aleg
   push offset sir
   call printf
   add esp,8 
   
 
   
   push offset tasta
   push offset int_format
   call scanf
   add esp,8
   
  
   		
	
	
   cmp tasta,1
   je adunare
   cmp tasta,2
   je scadere
   cmp tasta,3
   je scalar
  cmp tasta,4
   je transpusa
   cmp tasta,5
   je det
   cmp tasta, 6
   je terminare
   push offset incorect
   call printf
   add esp, 4
   jmp start_bucla_meniu
   
   adunare:
   ADUNARE_MATRICI matr1,matr2,lungime_matrice,lung_1
   jmp start_bucla_meniu
   
   scadere:
   SCADERE_MATRICI matr1,matr2,lungime_matrice,lung_1
   jmp start_bucla_meniu
   
   scalar:
   push offset scl
   push offset int_format
   call scanf
   add esp,8
   SCALAR_MATRICE matr1,scl, lungime_matrice,lung_1
   jmp start_bucla_meniu
    
   transpusa:
   TRANSPUSA_MATRICE matr1, lungime_matrice, lung_1
   jmp start_bucla_meniu
   
   det:
   DETERMINANT_2 matr1, lungime_matrice, lung_1
   jmp start_bucla_meniu
   
   jmp start_bucla_meniu
   terminare:
   push 0
   call exit
   end start
