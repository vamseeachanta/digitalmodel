
  <Surface mesh for KCS>

    kcs_bow1.dat: single block grid for forebody
    kcs_stn1.dat: single block grid for afterbody
    kcs_bow2.dat: two block grid for forebody
    kcs_stn2.dat: two block grid for afterbody

 <note>
   1. All files are in Tecplot format.
   2. *_*1.dat has grids upto DLWL(designed load water line).
   3. *.*2.dat has grids above DLWL.
   4. DLWL of KCS is located under the transom lower end.


   
 <Fortran read format>
      
      read(11,*)imax1,kmax1  <-- please remove tecplot headers.
      do k=1,kmax1
      do i=1,imax1
       read(11,*)xh1(i,k),yh1(i,k),zh1(i,k)
      enddo
      enddo
      if(file.eq.*_*2.grd)then  <-- if two-block grid
        read(11,*)imax2,kmax2   <-- please remove tecplot headers.
        do k=1,kmax2
        do i=1,imax2
        read(11,*)xh2(i,k),yh2(i,k),zh2(i,k)
        enddo
        enddo
      endif



 <Index configuration>

  For *_bow1.dat,
      i=1: bow profile line
      i=imax1: midship line
      k=1: keel line
      k=kmax1: Designed load water line (DLWL)

  For *_bow2.dat,
    1st block
      i=1: bulb bow line
      i=imax1: bulb neck line
      k=1: bottom bulb line
      k=kmax1: top bulb line
    2nd block
      i=1: bulb neck line + stem profile line
      i=imax2: midship line
      k=1: keel line
      k=kmax2: top water line above DLWL

  For *_stn1.dat,
      i=1: midship line
      i=imax1: stern profile line
      k=1: keel line
      k=kmax1: Designed load water line (DLWL)

  For *_stn2.dat,
    1st block
      i=1: overhang neck line
      i=imax1: transom end line
      k=1: profile line under transom overhang
      k=kmax1: top water line at transom overhang
    2nd block
      i=1: midship line
      i=imax2: overhang neck line + stern profile line
      k=1: keel line
      k=kmax2: top water line above DLWL


 <Coordinate system>

     X: downstream
     Y: toward starboard side
     Z: vertical upwards

     The origin of the coordinate system is located at the intersection
         point of midship, center plane and calm free surface.
     All coordinates are nondimensionalized by Lpp.
     Thus, X=-0.5: F.P.,  X=0.0: Midship,  X=0.5: A.P.
           Y=0.0: center plane(zero butock line)
           Z=0.0: calm water surface





