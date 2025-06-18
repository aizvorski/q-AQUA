program main
  use pes_shell

  implicit none
  real,dimension(:),allocatable::x
  real,dimension(:,:),allocatable::gd1,gd2,H
  character(len=32)::filename
  integer::i,j,natm,ierr
  character::symb
  real::eng
  real::eps
 
  call getarg(1,filename)
  open(21,file=trim(filename),status="old")

  read(21,*) natm
  allocate(x(3*natm), gd1(3,natm), gd2(3,natm),H(3*natm,3*natm))
  rewind(21)

  call pes_init(natm/3)
  do
     read(21,*,iostat=ierr) natm
     if (ierr < 0) exit
     read(21,*) 
     do i=1,natm
        read(21,*) symb,x(3*i-2:3*i)
     end do
     x = x / auang

     !H H...H O...O
     !Obtain energy,eng, and  energy gradient, gd1(3,natm)
     call pot_gd(x,eng,gd1)
     !call getpot(x,eng)
     write(*,*) "Potential energy in a.u., kcal/mol"
     write(*,'(2F15.8)')eng, eng*627.510

     write(*,*) "Energy gradient in a.u."
     do j=1,size(x)/3
       write(*,'(I4,3F15.8)')j, gd1(:,j)
     end do

    
end do
end program main
