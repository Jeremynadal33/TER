Module TER

  Implicit None
  Integer, Parameter :: PR=8
  Real(kind=PR),Parameter :: PI= 4.*atan(1.)
  
  Real(kind=PR),Parameter :: rho_s0=1, c_0 = 10 , alpha = 0.01 , beta = 0.1 , m_c=100.09 , m_s = 64.06 , A=1
  Integer,Parameter :: nbr_interval_x = 50
  Real(kind=PR),Parameter ::  delta_x=1._PR/nbr_interval_x, delta_t = 0.9*delta_x/100 , theta = 0.  
  

Contains

!!!!!!!!!!!------------------------------------ CALCUL AU TEMPS N+1  ---------------------------------------!!!!!!!!!!!!!!!!!

  
  !--------------------------CALCUL REEL-------------------------------!

  !---------------FONCTION PHI ET RHO -----------------!

  Function phi(c)Result(phi_c)
    Real(kind=PR),Dimension(0:nbr_interval_x),Intent(In) :: c
    Real(kind=PR),Dimension(0:nbr_interval_x) :: phi_c
    Integer :: m

    Do m=0,nbr_interval_x
       phi_c(m) = alpha * c(m) + beta
    End Do
    
  End Function phi

  Function rho(c,s)Result(rho_s)
    Real(kind=PR),Dimension(0:nbr_interval_x),Intent(In) :: c,s
    Real(kind=PR),Dimension(0:nbr_interval_x) :: rho_s,phi_c
    Integer :: m

    phi_c=phi(c)
    Do m=0,nbr_interval_x
       rho_s(m)= phi_c(m)*s(m)
    End Do
  End Function rho

  Function deltam(a,b,m)Result(delta_m)
    Real(kind=PR),Dimension(0:nbr_interval_x),Intent(In) :: a,b
    Real(kind=PR) :: delta_m
    Integer,Intent(In) :: m

    delta_m = (1./(2*delta_x**2))*( (a(m+1)+a(m))*(b(m+1)-b(m)) - (a(m)+a(m-1))*(b(m)-b(m-1)) )
    
  End Function deltam


  !----------------------FONCTION DU CAS REEL-----------------!
  
  Subroutine pas_1D_reel(c,rho_s,t,c_1,rho_s_1) 
    Real(kind=PR),Dimension(0:nbr_interval_x),Intent(In) :: c,rho_s
    Real(kind=PR),Intent(In) :: t
    Real(kind=PR),Dimension(0:nbr_interval_x),Intent(Out) :: c_1,rho_s_1
    Real(kind=PR),Dimension(0:nbr_interval_x) :: phi_c,phi_c_1,frac
    Integer :: m

    phi_c=phi(c)
    
    c_1(0)= c_0*exp(-rho_s0*t)
    Do m=1,nbr_interval_x-1
       c_1(m) = c(m) * exp(-delta_t*rho_s(m)) 
    End Do
    c_1(nbr_interval_x)=c_1(nbr_interval_x-1)
  
    Do m=0,nbr_interval_x
       frac(m)=rho_s(m)/phi_c(m)
    End Do
    
    rho_s_1(0)=rho_s0
    Do m=1,nbr_interval_x-1
       rho_s_1(m)= ( delta_t*deltam(phi_c,frac,m) + (1-delta_t*(1-theta)*c_1(m))*rho_s(m) ) / (1+ theta*delta_t*c_1(m))
       
    End Do
    rho_s_1(nbr_interval_x)=rho_s_1(nbr_interval_x-1)
    
  End Subroutine pas_1D_reel




  

  !-------------CAS PRELIMINAIRE ------------------------!
      Function pas_1D(vecteur_t,delta,PHI,nb_interval_x)Result(vecteur_tdt)
    Real(kind=PR),Dimension(0:nb_interval_x),Intent(In) :: vecteur_t
    Real(kind=PR),Intent(In) :: Delta,PHI
    Integer,Intent(In) :: nb_interval_x
    Real(kind=PR),Dimension(0:nb_interval_x) :: vecteur_tdt
    Integer :: m

    Do m=0,nb_interval_x
       If (m==0 .Or. m==nb_interval_x) Then
          vecteur_tdt(m)=0
       Else
          vecteur_tdt(m)= vecteur_t(m) + Delta *  PHI * (vecteur_t(m+1) + vecteur_t(m-1) - 2._PR*vecteur_t(m))  !!!!!!!! On calcul R1 (au temps n+1) à partir de R 
       End If
    End Do

  End Function pas_1D

  
  Function pas_2D(matrice_t,deltaX,deltaY,DeltaT,PHI,nb_interval_x,nb_interval_y)Result(matrice_tdt)
    Real(kind=PR),Dimension(0:nb_interval_x,0:nb_interval_y),Intent(In) :: matrice_t
    Real(kind=PR),Intent(In) :: DeltaX,DeltaY,DeltaT,PHI
    Integer,Intent(In) :: nb_interval_x,nb_interval_y
    Real(kind=PR),Dimension(0:nb_interval_x,0:nb_interval_y) :: matrice_tdt
    Integer :: m,n

    Do m=0,nb_interval_x
       Do n=0,nb_interval_y
          If (m==0 .Or. m==nb_interval_x .Or. n==0 .Or. n==nb_interval_y) Then
             matrice_tdt(m,n)=0
          Else
             matrice_tdt(m,n)= matrice_t(m,n) + &
                  PHI * deltaT * ( ( (matrice_t(m+1,n) - 2*matrice_t(m,n) + matrice_t(m-1,n) )/DeltaX**2 ) + &
                  ( (matrice_t(m,n+1) - 2*matrice_t(m,n) + matrice_t(m,n-1) )/DeltaY**2 ) ) 
          End If
       End Do
    End Do

  End Function pas_2D

  Function pas_3D(matrice_t,deltaX,deltaY,deltaZ,DeltaT,PHI,nb_interval_x,nb_interval_y,nb_interval_z)Result(matrice_tdt)
    Real(kind=PR),Dimension(0:nb_interval_x,0:nb_interval_y,0:nb_interval_z),Intent(In) :: matrice_t
    Real(kind=PR),Intent(In) :: DeltaX,DeltaY,DeltaZ,DeltaT,PHI
    Integer,Intent(In) :: nb_interval_x,nb_interval_y,nb_interval_z
    Real(kind=PR),Dimension(0:nb_interval_x,0:nb_interval_y,0:nb_interval_z) :: matrice_tdt
    Integer :: m,n,l

    Do m=0,nb_interval_x
       Do n=0,nb_interval_y
          Do l=0,nb_interval_z
             If (m==0 .Or. m==nb_interval_x .Or. n==0 .Or. n==nb_interval_y .Or. l==0 .Or. l==nb_interval_z) Then
                matrice_tdt(m,n,l)=0
             Else
                matrice_tdt(m,n,l)= matrice_t(m,n,l) + PHI * deltaT * &
                     ((matrice_t(m+1,n,l)-2*matrice_t(m,n,l)+matrice_t(m-1,n,l))/DeltaX**2) + ( &
                     ((matrice_t(m,n+1,l)-2*matrice_t(m,n,l)+matrice_t(m,n-1,l))/DeltaY**2) + &
                     ((matrice_t(m,n,l+1)-2*matrice_t(m,n,l)+matrice_t(m,n,l-1))/DeltaZ**2) ) 
             End If
          End Do

       End Do
    End Do
  End Function pas_3D


!!!!!!!!!!!------------------------------------ FIN CALCUL AU TEMPS N+1  ---------------------------------------!!!!!!!!!!!!!!!!!




!!!!!!!!!!!------------------------------ CALCUL DE L'ERREUR ------------------------------------!!!!!!!!!!!!!!!!!
  Function erreur_infinie_1D(solution_finale,solution_reelle,nb_interval_x)Result(erreur)
    Integer,Intent(In) :: nb_interval_x
    Real(kind=PR),Dimension(0:nb_interval_x),Intent(In) :: solution_finale 
    Real(kind=PR),Dimension(0:nb_interval_x),Intent(In) :: solution_reelle
    Real(kind=PR) :: erreur
    Integer :: m

    erreur=0
    Do m=0,nb_interval_x
       If (abs(solution_finale(m)-solution_reelle(m)) > erreur ) Then
          erreur = abs(solution_finale(m)-solution_reelle(m)) 
       End If
    End Do

  End Function erreur_infinie_1D

  Function erreur_1_1D(solution_finale,solution_reelle,nb_interval_x)Result(erreur)
    Integer,Intent(In) :: nb_interval_x
    Real(kind=PR),Dimension(0:nb_interval_x),Intent(In) :: solution_finale 
    Real(kind=PR),Dimension(0:nb_interval_x),Intent(In) :: solution_reelle
    Real(kind=PR) :: erreur,erreur_1
    Integer :: m

    erreur = 0 
    Do m=0,nb_interval_x
       erreur=erreur + abs(solution_finale(m)-solution_reelle(m))
    End Do
    erreur = erreur/nb_interval_x
  End Function erreur_1_1D
!!!!!!!!!!!------------------------------FIN CALCUL DE L'ERREUR ------------------------------------!!!!!!!!!!!!!!!!!

!!!!!!!!!------------------------------ORDRE DE LA METHODE ----------------------------------!!!!!!!!!!!!!!!!!!
  Function ordre_1D()Result(som_ordre)
    Integer :: nb_interval_x
    Real(kind=PR) :: deltaX,deltaT,delta,PHI,t,T_final,erreur,erreur_1,ordre,som_ordre
    Integer :: m,n
    Real(kind=PR),Dimension(:),Allocatable :: R,R1,R_sol_reelle

    delta=0.1
    nb_interval_x = 10
    deltaX=1./nb_interval_x
    deltaT= delta*deltaX**2
    PHI=1
    t=0
    T_final = 100*deltaT
    ordre=0
    som_ordre=0

    Open(unit=1,file='ordre_delta_x.txt')

    Do n=1,10        ! 5 calcul d'erreur afin de faire une moyenne sur 4 alpha (ordre)
       Allocate (R(0:nb_interval_x))
       Allocate (R1(0:nb_interval_x))
       Allocate (R_sol_reelle(0:nb_interval_x))

!!!!! T_initial 
       Do m=0,(nb_interval_x)            
          If (m==0 .Or. m==nb_interval_x) Then
             R(m)=0
          Else 
             R(m)=sin(2*PI*m*deltaX)                                       !Solution au temps initial
             R_sol_reelle(m)=sin(PI*m*deltaX*2)*exp(-((PI**2)*4*T_final))  !Solution au temps final
          End If
       End Do
       
!!!!!!T_final
       Do while (t<T_final)
          R1= pas_1D(R,delta,PHI,nb_interval_x)
          R=R1
          t=t+deltaT
       End Do

       If (n>1) Then
          erreur = erreur_1_1D(R1,R_sol_reelle,nb_interval_x)
          ordre = (1./log(2.))*log(erreur_1/erreur)
          som_ordre = som_ordre + ordre
          Write(1,*) deltaX,erreur,ordre
          

          erreur_1=erreur
       Else
          erreur_1=erreur
          write(1,*) deltaX,erreur
          
       End If
       Deallocate(R,R1,R_sol_reelle)
       
!!!!On réinitialise les parametres pour la boucle suivante
       nb_interval_x=nb_interval_x*2
       print *, nb_interval_x
       deltaX=1./nb_interval_x
       deltaT= delta*deltaX**2
       PHI=1
       t=0
       T_final = 100*deltaT
    End Do

    som_ordre = som_ordre/10




    Close(1)
  End Function ordre_1D




!!!!!!!!!------------------------------FIN ORDRE DE LA METHODE ----------------------------------!!!!!!!!!!!!!!!!!!



End Module TER

!from film import *
!film1D('results_1D_dir_dir/solution_', '.txt', 0, 2000, -0.1, 0.0) 
