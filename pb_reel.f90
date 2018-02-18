Program pb_reel
  Use TER
  Use mod_printmat
  Implicit None

  !---------------------- CONDITIONS INITIALES ---------------------------!
  Real(kind=PR),Dimension(0:nbr_interval_x) :: c,s,c_1,s_1
  Real(kind=PR),Dimension(0:nbr_interval_x) :: rho_s,phi_c,rho_s_1
  Real(kind=PR) :: t,tmax
  
  Character(len=20) :: FileName_c,FileName_s
  Integer :: num_fichier
  Integer :: m



  !----------INITIALISATION 
  num_fichier = 1000
  t=0._PR
  tmax=0.5
  
  Open(unit=1,file='c_theta_0.txt')
  Open(unit=2,file='rho_theta_0.txt')

  !----------T=0

  s=0._PR
  c=c_0
  phi_c=phi(c)

  Do m=0,nbr_interval_x
     rho_s(m)=phi_c(m)*s(m)
  End Do
  

  !Do m=0,nbr_interval_x
  !   Write(1,*) m*delta_x,phi_c(m)
  !   Write(2,*) m*delta_x,s(m)
  !End Do

  Do while (t<tmax)
     t=t+delta_t     
     Call pas_1D_reel(c,rho_s,t,c_1,rho_s_1)
     rho_s=rho_s_1
     c=c_1
  End Do


  
  Do m=0,nbr_interval_x
     Write(1,*) m*delta_x,c(m)
     Write(2,*) m*delta_x,rho_s(m)
  End Do
  

  Close(1)
  Close(2)
End Program pb_reel
