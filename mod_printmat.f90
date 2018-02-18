module mod_printmat
  ! ce module contient la routine 
  ! printmat : call printmat(A,[fid,[matname]])
  ! qui permet d'écrire des matrices entières, réelles et complexes
  ! dans le descripteur de fichier 'fid' (0=entrée standard)
  ! une ligne est de type 'flc a(i,1) sep a(i,2) sep ... lrc'
  ! où flc (first left character), sep ( separator ), lrc (last right character )
  ! sont définis ci_dessous
  implicit none
  character(len=*),private,parameter :: flc = '['
  character(len=*),private,parameter :: lrc = '];'
  character(len=*),private,parameter :: sep = ','
  interface printmat
    module procedure pmi,pmr4,pmr8,pmc4,pmc8
  end interface

contains

  subroutine pmi(A,fid,matname) ! permet d'afficher une matrice entière
    integer,dimension(:,:),intent(in) :: A ! variable d'entrée
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid
    character(len=*),optional :: matname

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif

    if (present(matname)) then
      write(fid_loc,'(a,1x,a,1x,i0,a,i0,a)') '# Affichage de la matrice ',matname,'de taille [',size(A,1),',',size(A,2),']'
    else
      write(fid_loc,'(a,i0,a,i0,a)') '# Affichage d''une matrice entière de taille [',size(A,1),',',size(A,2),']'
    endif
    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'(i0,a,1x)',advance='no') A(i,j),sep
      enddo
      write(fid_loc,'(i0,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') lrc 
    enddo
  end subroutine

  subroutine pmr4(A,fid,matname) ! permet d'afficher une matrice réelle simple précision
    real(kind=4),dimension(:,:),intent(in) :: A
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid
    character(len=*),optional :: matname

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif
    if (present(matname)) then
      write(fid_loc,'(a,1x,a,1x,i0,a,i0,a)') '# Affichage de la matrice ',matname,'de taille [',size(A,1),',',size(A,2),']'
    else
      write(fid_loc,'(a,i0,a,i0,a)') '# Affichage d''une matrice réelle de taille [',size(A,1),',',size(A,2),']'
    endif
    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'(es10.3,a,1x)',advance='no') A(i,j),sep
      enddo
      write(fid_loc,'(es10.3,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine

  subroutine pmr8(A,fid,matname) ! permet d'afficher une matrice réelle double précision
    real(kind=8),dimension(:,:),intent(in) :: A
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid
    character(len=*),optional :: matname

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif
    if (present(matname)) then
      write(fid_loc,'(a,1x,a,1x,i0,a,i0,a)') '# Affichage de la matrice ',matname,'de taille [',size(A,1),',',size(A,2),']'
    else
      write(fid_loc,'(a,i0,a,i0,a)') '# Affichage d''une matrice réelle de taille [',size(A,1),',',size(A,2),']'
    endif
    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'(es10.3,a,1x)',advance='no') A(i,j),sep
      enddo
      write(fid_loc,'(es10.3,1x)',advance='no') A(i,n2)
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine

  subroutine pmc4(A,fid,matname) ! permet d'afficher une matrice complex simple précision
    complex(kind=4),dimension(:,:),intent(in) :: A 
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid
    character(len=*),optional :: matname

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif      
    if (present(matname)) then
      write(fid_loc,'(a,1x,a,1x,i0,a,i0,a)') '# Affichage de la matrice ',matname,'de taille [',size(A,1),',',size(A,2),']'
    else
      write(fid_loc,'(a,i0,a,i0,a)') '# Affichage d''une matrice complexe de taille [',size(A,1),',',size(A,2),']'
    endif
    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'("(",es10.3,",",es10.3,")",a,1x)',advance='no') real(A(i,j)),aimag(A(i,j)),sep
      enddo
      write(fid_loc,'("(",es10.3,",",es10.3,")")',advance='no') real(A(i,n2)),aimag(A(i,n2))
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine

  subroutine pmc8(A,fid,matname) ! permet d'afficher une matrice complexe double précision
    complex(kind=8),dimension(:,:),intent(in) :: A
    integer :: i,j,n2,fid_loc
    integer,optional,intent(in) :: fid
    character(len=*),optional :: matname

    n2 = size(A,2)
    if (.not.present(fid)) then
      fid_loc = 0
    else
      fid_loc=fid
    endif        
    if (present(matname)) then
      write(fid_loc,'(a,1x,a,1x,i0,a,i0,a)') '# Affichage de la matrice ',matname,'de taille [',size(A,1),',',size(A,2),']'
    else
      write(fid_loc,'(a,i0,a,i0,a)') '# Affichage d''une matrice complexe de taille [',size(A,1),',',size(A,2),']'
    endif
    do i=1,size(A,1)
      write(fid_loc,'(a,1x)',advance='no') flc
      do j=1,n2 -1 
        write(fid_loc,'("(",es10.3,",",es10.3,")",a,1x)',advance='no') real(A(i,j)),aimag(A(i,j)),sep
      enddo
      write(fid_loc,'("(",es10.3,",",es10.3,")")',advance='no') real(A(i,n2)),aimag(A(i,n2))
      write(fid_loc,'(a)') lrc 
    end do
  end subroutine
end module