program contest
!tipo 1=solitario; tipo 2= rainha; tipo 3 = operária

use squarelattice

implicit none

integer, parameter :: size=400,LL=20,carry_capacity=100,passos=10000,qens=1,m0=3
integer, parameter :: seed = 86456
real,parameter :: death_rate=0.05,birth_rate=0.10,pvalue=0.05,qs=0.50

integer, dimension(1:size,1:4) :: site_occup !site_occup(i,k) é o numero de indinviduos do tipo k no sitio i, k=4 é a quantidade total
integer, dimension(1:size*carry_capacity,1:2) :: state   !state(i,1) é o tipo do individuo i e state(i,2) é o sitio do individuo i 
integer, dimension(1:size) :: queens  !queens(i) é a quantidade de rainhas do sítio i

integer :: n_q,n_o,ntotal,nocu,n,ens,iqs,ns,nqueens,nsoli,noper
integer :: evo,mcstp,i,j,s,site_chos,ind,novo,sum_q,z
real :: q_l,pext
!real :: qs,death_rate,birth_rate,br1,br2,dr1,dr2
character(5) :: aux1,aux2,aux3,aux4,aux5

open(93,file='../results/typesxtime.txt')

call srand(seed)

! constroi a rede, gerando lista(i,j), onde i é o sítio e lista(i,j) para j=1,2,3,4 são os vizinhos 
call buildnet(LL,size)

 !Distribuição inicial de estratégias

do 40 ens=1,qens

call player_state_ini(pvalue)

do 20 evo=1,passos
  write(93,*)evo,nsoli,(nqueens+noper) !printo o tempo os solitarios e os sociais
	do 30 mcstp=1,size
          ind=int(rand()*nocu)+1 
          site_chos=ind
          if(evo .eq. 500) then !tipo solitário estacionário
           do s=1,4
             if(site_occup(lista(site_chos,s),4).eq.0) then ! invasão do tipo social, tipo 2		  		    
	        site_occup(lista(site_chos,s),2)=site_occup(lista(site_chos,s),2)+1
	        site_occup(lista(site_chos,s),4)=site_occup(lista(site_chos,s),4)+1                   
	        nocu=nocu+1
                nqueens=nqueens+1
	        ntotal=ntotal+1
                exit			    
             endif
           enddo
          endif
             if(rand().lt.birth_rate) then	!escolhe alguem para reproduzir
	        if(site_occup(site_chos,1).eq. 1) then  !vejo se é solitário
                   do j=1,4
 	             if(site_occup(lista(site_chos,j),4).eq.0) then  ! o filho deve dispersar	  
	                site_occup(lista(site_chos,j),1)=site_occup(lista(site_chos,j),1)+1
	                site_occup(lista(site_chos,j),4)=site_occup(lista(site_chos,j),4)+1                   
	                nocu=nocu+1
                        nsoli=nsoli+1
	                ntotal=ntotal+1			    
                     endif
                   enddo
                else
	           !if(site_occup(site_chos,4).lt.m0) then !possibilidade de incrementar os beneficios dos sociais
                   !  birth_rate=br1
                   !  death_rate=dr1
                   !  else
                   !  birth_rate=br2
                   !  death_rate=dr2
                  ! endif
 
                  if(rand().le.qs) then !com probabilidade de qs a filha é operária
                     if(site_occup(site_chos,4).lt.carry_capacity) then 
                        site_occup(site_chos,3)=site_occup(site_chos,3)+1
		        site_occup(site_chos,4)=site_occup(site_chos,4)+1
                        noper=noper+1 		    
                        ntotal=ntotal+1
                     endif
                  else    !com probabilidade de 1-qs a filha é rainha
                     do j=1,4
 	                if(site_occup(lista(site_chos,j),4).eq.0) then 				  
	                   site_occup(lista(site_chos,j),2)=site_occup(lista(site_chos,j),2)+1
	                   site_occup(lista(site_chos,j),4)=site_occup(lista(site_chos,j),4)+1
	                   nocu=nocu+1
                           nqueens=nqueens+1
	                   ntotal=ntotal+1			    
	                 endif
                     enddo
                  endif
                endif
             endif

             if(rand().lt.death_rate) then ! escolho alguem para morrer
                 if(site_occup(site_chos,1).eq. 1) then !verifico se é solitário
	            nocu=nocu-1
                    nsoli=nsoli-1
                    ntotal=ntotal-1 
		    site_occup(site_chos,1)=0 
		    site_occup(site_chos,4)=0
                 else           !verifico se é social
                    noper=noper-site_occup(site_chos,3) 
                    ntotal=ntotal-site_occup(site_chos,4)                 
	            nocu=nocu-1
                    nqueens=nqueens-1
		    site_occup(site_chos,2)=0 
                    site_occup(site_chos,3)=0
		    site_occup(site_chos,4)=0 
                endif
	     endif 

	30 continue         
20 continue

40 enddo

contains

subroutine player_state_ini(pvalue)
    implicit none
    real, intent(in) :: pvalue 
      site_occup=0
      ntotal=0
      nocu=0
      noper=0
      nqueens=0
      nsoli=0

       site_occup(1,1)=site_occup(1,1)+1   !começo com um único indivídio solitário
       site_occup(1,4)=site_occup(1,4)+1
       ntotal=ntotal+1 
       nocu=nocu+1
       nsoli=nsoli+1

end subroutine 

end program contest
