Module ecrire_fichier
  Implicit None
  Integer,Parameter,Private :: PR=8

Contains
  

  !-----------------------------POUR ECRIRE 1D ---------------------------------!

  Subroutine ecrire_tableau(FileName,tableau,nb_interval_x,deltaX)  
    Character(len=20),Intent(In) :: FileName                 !!!!!!!! COMMENT SAUVEGARDER DANS UN AUTRE REPERTOIRE ????
    Integer, Intent(In) :: nb_interval_x
    Real(kind=PR),Dimension(0:nb_interval_x),Intent(In) :: tableau
    Real(kind=PR),Intent(In) :: deltaX
    Integer :: i


    Open(file=FileName, unit=1)  !!! Pour ecrire dans un repertoir : 'Data/FileName' mais ici FileName variable ...
    Do i=0,nb_interval_x
       Write(1,*) i*deltaX,tableau(i)
    End Do
    Close(1)

  End Subroutine ecrire_tableau

  !-----------------------------POUR ECRIRE 2D ---------------------------------!

  Subroutine ecrire_matrice(FileName,matrice,nb_interval_x,nb_interval_y)
    Character(len=20),Intent(In) :: FileName
    Integer, Intent(In) :: nb_interval_x,nb_interval_y
    Real(kind=PR),Dimension(0:nb_interval_x,0:nb_interval_y),Intent(In) :: matrice
    Integer :: i

    Open(unit=1,file = FileName)
    Do i=0,nb_interval_x
       write(1,*) matrice(i,:)

    End Do
    Close(1)

  End Subroutine ecrire_matrice

  !splot 'data_100.txt' matrix using 1:2:3 w lines
  !do for [t=1000:2000]{set yrange[0:1];set zrange [0:1];set output 'image_'.t.'.jpeg';set title 'Temperature d une surface au cours du temps';splot 'data_'.t.'.txt' matrix using 1:2:3 w l }

  !-----------------------------POUR ECRIRE 3D ---------------------------------!
  Subroutine ecrire_matrice_3D(FileName,matrice,nb_interval_x,nb_interval_y,nb_interval_z,deltaX,deltaY,deltaZ)
    Character(len=20),Intent(In) :: FileName
    Integer, Intent(In) :: nb_interval_x,nb_interval_y,nb_interval_z
    Real(kind=PR),Dimension(0:nb_interval_x,0:nb_interval_y,0:nb_interval_z),Intent(In) :: matrice
    Real(kind=PR),Intent(In) :: deltaX,deltaY,deltaZ
    Integer :: i,j,k

    Open(unit=1,file=FileName)
    Do i = 0,nb_interval_x
       Do j = 0,nb_interval_y
          Do k = 0, nb_interval_z
             write(1,*) i*deltaX,j*deltaY,k*deltaZ,matrice(i,j,k)
          End Do
       End Do
       write(1,*) 
    End Do

    Close(1)

  End Subroutine ecrire_matrice_3D

  !set pm3d
  !sp "data_100.txt" u 1:2:3:4 w pm3d

!!!!!!!!!!!------------------------------FIN ECRIRE DANS FICHIER---------------------------------!!!!!!!!!!!!!!!!
End Module ecrire_fichier
