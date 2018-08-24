      PROGRAM xnewt
C     driver for routine newt
      INTEGER N
      PARAMETER(N=4)
      INTEGER i
      REAL x(N),f(N)
      LOGICAL check
c      x(1)=2.
c      x(2)=0.5
      x(1)=237.
      x(2)=16.
      x(3)=160.
      x(4)=0.036
      call newt(x,N,check)
      call funcv(N,x,f)
      if (check) then
        write(*,*) 'Convergence problems.'
      endif
      write(*,'(1x,a5,t10,a1,t22,a1)') 'Index','x','f'
      do 11 i=1,N
        write(*,'(1x,i2,2x,2f12.6)') i,x(i),f(i)
11    continue
	READ(*,*)
      END

      SUBROUTINE funcv(n,x,f)
      INTEGER n
      REAL x(n),f(n)
c	 f(1)=x(1)**2+x(2)**2-2.
c	 f(2)=exp(x(1)-1.)+x(2)**3-2.
	Pi = DACOS(-1.D0);
	P0 = 5000000.
	a =	30.
	b0 = 200.
	b1 = 0.2114
	b2 = 3.958
	b3 = 2.464
	D0 = 10.714
	c = 1.5
	T0 = 303.
	Gr = 1000.
	Cp = 1007.
	xi = 0.13
	rho = 1.148
	eta = 0.75
	alpha = 0.549
	beta = 13.6
	g = 9.80665

	A1 = T0*(1.+xi)*g*x(3)*alpha*Gr*beta
	A2 = T0*x(1)**2.*(1.+xi)*(3.*T0*beta**2.*x(1)**2.*xi+3.*T0
	&*beta**2.*x(1)**2.+4.*3.**(1./2.)*2.**(1./2.)*(T0*(1.+xi)*g*x(3)
     &*alpha*Gr*beta)**(1./2.)*rho*Cp*x(2)**2.)
	A3 = (log(x(2))-log(D0))**(-b3)



      f(1)=-.4413190690e-5*x(3)*Pi*x(1)/(1.700000000*Pi*x(1)**2.
  	&+.4564582881e-2*3.**(1./2.)*(Pi*x(1)**2.*(46235.37600*Pi*x(1)
     &**2.+5711880.932*6.**(1./2.)*x(3)**(1./2.)*Pi*x(2)**2.))**
     &(1./2.))*(3399.660000*Pi*x(1)**2.-9.128252845*3.**(1./2.)*(Pi
     &*x(1)**2.*(46235.37600*Pi*x(1)**2.+5711880.932*6.**(1./2.)*
     &x(3)**(1./2.)*Pi*x(2)**2.))**(1./2.))+.2206595345e-5*x(3)*Pi
     &*x(1)**2./(1.700000000*Pi*x(1)**2.+.4564582881e-2*3.**(1./2.)
     &*(Pi*x(1)**2.*(46235.37600*Pi*x(1)**2.+5711880.932*6.**(1./2.)
     &*x(3)**(1./2.)*Pi*x(2)**2.))**(1./2.))**2.*(3399.660000*Pi*x(1)
     &**2.-9.128252845*3.**(1./2.)*(Pi*x(1)**2.*(46235.37600*Pi*x(1)
     &**2.+5711880.932*6.**(1./2.)*x(3)**(1./2.)*Pi*x(2)**2.))**
     &(1./2.))*(3.400000000*Pi*x(1)+.2282291441e-2*3.**(1./2.)
     &/(Pi*x(1)**2.*(46235.37600*Pi*x(1)**2.+5711880.932*6.**
     &(1./2.)*x(3)**(1./2.)*Pi*x(2)**2.))**(1./2.)*(2.*Pi*x(1)*
     &(46235.37600*Pi*x(1)**2.+5711880.932*6.**(1./2.)*x(3)**(1.
     &/2.)*Pi*x(2)**2.)+92470.75200*Pi**2.*x(1)**3.))-.2206595345e-5
     &*x(3)*Pi*x(1)**2./(1.700000000*Pi*x(1)**2.+.4564582881e-2*3.**
     &(1./2.)*(Pi*x(1)**2.*(46235.37600*Pi*x(1)**2.+5711880.932*6.
     &**(1./2.)*x(3)**(1./2.)*Pi*x(2)**2.))**(1./2.))*(6799.320000*
     &Pi*x(1)-4.564126423*3.**(1./2.)/(Pi*x(1)**2.*(46235.37600*Pi*
     &x(1)**2.+5711880.932*6.**(1./2.)*x(3)**(1./2.)*Pi*x(2)**2.))
     &**(1./2.)*(2.*Pi*x(1)*(46235.37600*Pi*x(1)**2.+5711880.932*6.
     &**(1./2.)*x(3)**(1./2.)*Pi*x(2)**2.)+92470.75200*Pi**2.*x(1)
     &**3.))+.2452855968e-17*(3399.660000*Pi*x(1)**2.-9.128252845
     &*3.**(1./2.)*(Pi*x(1)**2.*(46235.37600*Pi*x(1)**2.+5711880.932
     &*6.**(1./2.)*x(3)**(1./2.)*Pi*x(2)**2.))**(1./2.))**2./Pi**2.
     &/x(2)**4.*(6799.320000*Pi*x(1)-4.564126423*3.**(1./2.)/
     &(Pi*x(1)**2.*(46235.37600*Pi*x(1)**2.+5711880.932*6.**(1./2.)
     &*x(3)**(1./2.)*Pi*x(2)**2.))**(1./2.)*(2.*Pi*x(1)*(46235.37600
     &*Pi*x(1)**2.+5711880.932*6.**(1./2.)*x(3)**(1./2.)*Pi*x(2)
     &**2.)+92470.75200*Pi**2.*x(1)**3.))-15.*x(4)*Pi*x(1)

  	f(1)=-3.*x(1)**3.*(-2.*g*x(3)*alpha*Gr*Cp*rho*x(2)**2.*(A2)
	&**(1./2.)*3.**(1./2.)*2.**(1./2.)*(A1)**(1./2.)-2.*g*x(3)*
     &alpha*Gr*Cp*rho*x(2)**2.*(A2)**(1./2.)*xi*3.**(1./2.)*2.**(
     &1./2.)*(A1)**(1./2.)+2.*x(4)*a*x(2)**2.*rho*Cp**2.*T0*(A2)
     &**(1./2.)*xi*3.**(1./2.)*2.**(1./2.)*(A1)**(1./2.)+2.*x(4)
     &*a*x(2)**2.*rho*Cp**2.*T0*(A2)**(1./2.)*3.**(1./2.)*2.**(1
     &./2.)*(A1)**(1./2.)+9.*x(4)*a*Cp*T0**3.*x(1)**4.*beta**3.*
     &3.**(1./2.)*xi+3.*x(4)*a*Cp*T0**3.*x(1)**4.*beta**3.*3.**(
     &1./2.)+9.*x(4)*a*Cp*T0**3.*x(1)**4.*beta**3.*3.**(1./2.)*x
     &i**2.+3.*x(4)*a*Cp*T0**3.*x(1)**4.*beta**3.*xi**3.*3.**(1.
     &/2.)+6.*x(4)*a*Cp*T0**2.*(A2)**(1./2.)*beta**2.*x(1)**2.*x
     &i+12.*x(4)*a*x(2)**2.*rho*Cp**2.*T0**2.*x(1)**2.*beta*xi**
     &2.*2.**(1./2.)*(A1)**(1./2.)+24.*x(4)*a*x(2)**2.*rho*Cp**2
     &.*T0**2.*x(1)**2.*beta*xi*2.**(1./2.)*(A1)**(1./2.)+12.*T0
     &*x(1)**2.*g*x(3)*alpha*Gr*Cp*rho*x(2)**2.*xi**2.*2.**(1./2.
     &)*(A1)**(1./2.)*beta+12.*x(4)*a*x(2)**2.*rho*Cp**2.*T0**2.
     &*x(1)**2.*beta*2.**(1./2.)*(A1)**(1./2.)+12.*T0*x(1)**2.*g
     &*x(3)*alpha*Gr*Cp*rho*x(2)**2.*beta*2.**(1./2.)*(A1)**(1./2
     &.)+24.*T0*x(1)**2.*g*x(3)*alpha*Gr*Cp*rho*x(2)**2.*beta*xi*
     &2.**(1./2.)*(A1)**(1./2.)+3.*x(4)*a*Cp*T0**2.*(A2)**(1./2.
     &)*beta**2.*x(1)**2.*xi**2.-6.*(A2)**(1./2.)*g*x(3)*alpha*Gr
     &*T0*xi*beta**2.*x(1)**2.-3.*(A2)**(1./2.)*g*x(3)*alpha*Gr*T
     &0*beta**2.*x(1)**2.+3.*x(4)*a*Cp*T0**2.*(A2)**(1./2.)*beta
     &**2.*x(1)**2.-3.*(A2)**(1./2.)*g*x(3)*alpha*Gr*T0*xi**2.*be
     &ta**2.*x(1)**2.+9.*T0**2.*x(1)**4.*g*x(3)*alpha*Gr*3.**(1./
     &2.)*beta**3.*xi+3.*T0**2.*x(1)**4.*g*x(3)*alpha*Gr*xi**3.*3
     &.**(1./2.)*beta**3.+9.*T0**2.*x(1)**4.*g*x(3)*alpha*Gr*xi**
     &2.*3.**(1./2.)*beta**3.+3.*T0**2.*x(1)**4.*g*x(3)*alpha*Gr*
     &3.**(1./2.)*beta**3.)*Pi/Cp/(A2)**(1./2.)/(3.*beta*x(1)**2
     &.*T0+3.*beta*x(1)**2.*T0*xi+3.**(1./2.)*(A2)**(1./2.))**2.

	f(2)=6.*x(1)**2.*x(3)*T0*Pi/x(2)/Cp/log(x(2)/D0)/(3.*beta*x
	&(1)**2.*T0+3.*beta*x(1)**2.*T0*xi+3.**(1./2.)*(A2)**(1./2.)
     &)**2./(A2)**(1./2.)*(3.*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b0*be
     &ta**2.*x(1)**2.*xi**2.*log(D0)+12.*x(4)*T0*x(1)**2.*x(2)**3
     &.*rho*Cp**2.*b0*beta*2.**(1./2.)*(A1)**(1./2.)*log(D0)-2.*2
     &.**(1./2.)*x(4)*(A2)**(1./2.)*x(2)**3.*rho*Cp**2.*b0*3.**(1
     &./2.)*(A1)**(1./2.)*log(x(2))+12.*x(4)*T0*x(1)**2.*x(2)**3.
     &*rho*Cp**2.*b0*xi**2.*beta*2.**(1./2.)*(A1)**(1./2.)*log(D0
     &)+9.*A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b1*100.**(-b2)*x(3)**b
     &2*b3*beta**3.*3.**(1./2.)*xi+3.*3.**(1./2.)*g*alpha*Gr*T0*x(
     &1)**6.*beta**3.*xi*log(x(2))-3.*3.**(1./2.)*g*alpha*Gr*T0*x(
     &1)**6.*beta**3.*xi*log(D0)-3.*3.**(1./2.)*x(4)*T0**2.*x(1)*
     &*4.*x(2)*Cp*b0*beta**3.*log(x(2))+3.*3.**(1./2.)*x(4)*T0**2
     &.*x(1)**4.*x(2)*Cp*b0*beta**3.*log(D0)-3.*A3*x(4)*T0**2.*x(
     &1)**4.*x(2)*Cp*b1*100.**(-b2)*x(3)**b2*beta**3.*xi**3.*3.**
     &(1./2.)*log(x(2))+9.*A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b1*100
     &.**(-b2)*x(3)**b2*b3*beta**3.*3.**(1./2.)*xi**2.+24.*A3*x(4
     &)*T0*x(1)**2.*x(2)**3.*rho*Cp**2.*b1*100.**(-b2)*x(3)**b2*b
     &3*beta*xi*2.**(1./2.)*(A1)**(1./2.)+g*alpha*Gr*T0*x(1)**6.*b
     &eta**3.*3.**(1./2.)*xi**3.*log(x(2))-g*alpha*Gr*T0*x(1)**6.*
     &beta**3.*3.**(1./2.)*xi**3.*log(D0)-9.*x(4)*T0**2.*x(1)**4.
     &*x(2)*Cp*b0*xi**2.*beta**3.*3.**(1./2.)*log(x(2))+9.*x(4)*T
     &0**2.*x(1)**4.*x(2)*Cp*b0*xi**2.*beta**3.*3.**(1./2.)*log(D
     &0)+g*alpha*Gr*T0*x(1)**6.*beta**3.*3.**(1./2.)*log(x(2))-g*a
     &lpha*Gr*T0*x(1)**6.*beta**3.*3.**(1./2.)*log(D0)+6.*A3*x(4)*
     &(A2)**(1./2.)*x(2)*Cp*T0*b1*100.**(-b2)*x(3)**b2*beta**2.*x
     &(1)**2.*xi*log(D0)+2.*x(1)**4.*g*alpha*Gr*beta**2.*(A2)**(1.
     &/2.)*xi*log(D0)+9.*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b0*beta**3.
     &*xi*3.**(1./2.)*log(D0)+3.*3.**(1./2.)*g*alpha*Gr*T0*x(1)**6
     &.*beta**3.*xi**2.*log(x(2))-3.*3.**(1./2.)*x(4)*T0**2.*x(1)
     &**4.*x(2)*Cp*b0*xi**3.*beta**3.*log(x(2))-3.*3.**(1./2.)*g*
     &alpha*Gr*T0*x(1)**6.*beta**3.*xi**2.*log(D0)+3.*3.**(1./2.)*
     &x(4)*T0**2.*x(1)**4.*x(2)*Cp*b0*xi**3.*beta**3.*log(D0)+3.*
     &A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b1*100.**(-b2)*x(3)**b2*bet
     &a**3.*xi**3.*3.**(1./2.)*log(D0)-12.*A3*x(4)*T0*x(1)**2.*x(
     &2)**3.*rho*Cp**2.*b1*100.**(-b2)*x(3)**b2*beta*2.**(1./2.)*
     &(A1)**(1./2.)*log(x(2))-x(1)**4.*g*alpha*Gr*beta**2.*(A2)**(
     &1./2.)*log(x(2))-9.*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b0*beta**3
     &.*xi*3.**(1./2.)*log(x(2))-12.*A3*x(4)*T0*x(1)**2.*x(2)**3.
     &*rho*Cp**2.*b1*100.**(-b2)*x(3)**b2*beta*xi**2.*2.**(1./2.)
     &*(A1)**(1./2.)*log(x(2))+3.*A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp
     &*b1*100.**(-b2)*x(3)**b2*b3*beta**3.*3.**(1./2.)+2.*A3*x(4)
     &*(A2)**(1./2.)*x(2)**3.*rho*Cp**2.*b1*100.**(-b2)*x(3)**b2*
     &b3*xi*3.**(1./2.)*2.**(1./2.)*(A1)**(1./2.)+4.*g*alpha*Gr*Cp
     &*rho*x(2)**2.*x(1)**4.*beta*2.**(1./2.)*(A1)**(1./2.)*log(x
     &(2))-x(1)**4.*g*alpha*Gr*beta**2.*(A2)**(1./2.)*xi**2.*log(x
     &(2))-3.*A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b1*100.**(-b2)*x(3)
     &**b2*beta**3.*3.**(1./2.)*log(x(2))+8.*g*alpha*Gr*Cp*rho*x(2
     &)**2.*x(1)**4.*beta*xi*2.**(1./2.)*(A1)**(1./2.)*log(x(2))+
     &2.*A3*x(4)*(A2)**(1./2.)*x(2)**3.*rho*Cp**2.*b1*100.**(-b2)
     &*x(3)**b2*b3*3.**(1./2.)*2.**(1./2.)*(A1)**(1./2.)-12.*x(4)
     &*T0*x(1)**2.*x(2)**3.*rho*Cp**2.*b0*beta*2.**(1./2.)*(A1)**
     &(1./2.)*log(x(2))+3.*A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b1*100
     &.**(-b2)*x(3)**b2*beta**3.*3.**(1./2.)*log(D0)+x(1)**4.*g*a
     &lpha*Gr*beta**2.*(A2)**(1./2.)*log(D0)+x(1)**4.*g*alpha*Gr*be
     &ta**2.*(A2)**(1./2.)*xi**2.*log(D0)-24.*x(4)*T0*x(1)**2.*x(
     &2)**3.*rho*Cp**2.*b0*beta*xi*2.**(1./2.)*(A1)**(1./2.)*log(
     &x(2))+2.*A3*x(4)*(A2)**(1./2.)*x(2)**3.*rho*Cp**2.*b1*100.*
     &*(-b2)*x(3)**b2*3.**(1./2.)*2.**(1./2.)*(A1)**(1./2.)*log(D
     &0)-4.*g*alpha*Gr*Cp*rho*x(2)**2.*x(1)**4.*beta*2.**(1./2.)*(
     &A1)**(1./2.)*log(D0)+3.*A3*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b1
     &*100.**(-b2)*x(3)**b2*b3*beta**2.*x(1)**2.*xi**2.+2.*2.**(1
     &./2.)*x(4)*(A2)**(1./2.)*x(2)**3.*rho*Cp**2.*b0*3.**(1./2.)
     &*(A1)**(1./2.)*log(D0)-2.*x(1)**4.*g*alpha*Gr*beta**2.*(A2)*
     &*(1./2.)*xi*log(x(2))+12.*A3*x(4)*T0*x(1)**2.*x(2)**3.*rho*
     &Cp**2.*b1*100.**(-b2)*x(3)**b2*b3*xi**2.*beta*2.**(1./2.)*(
     &A1)**(1./2.)-6.*A3*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b1*100.**(
     &-b2)*x(3)**b2*beta**2.*x(1)**2.*xi*log(x(2))-3.*x(4)*(A2)**
     &(1./2.)*x(2)*Cp*T0*b0*beta**2.*x(1)**2.*xi**2.*log(x(2))-3.
     &*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b0*beta**2.*x(1)**2.*log(x(2
     &))+12.*A3*x(4)*T0*x(1)**2.*x(2)**3.*rho*Cp**2.*b1*100.**(-b
     &2)*x(3)**b2*beta*2.**(1./2.)*(A1)**(1./2.)*log(D0)+3.*A3*x(
     &4)*(A2)**(1./2.)*x(2)*Cp*T0*b1*100.**(-b2)*x(3)**b2*beta**2
     &.*x(1)**2.*log(D0)+6.*A3*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b1*1
     &00.**(-b2)*x(3)**b2*b3*beta**2.*x(1)**2.*xi+4.*g*alpha*Gr*Cp
     &*rho*x(2)**2.*x(1)**4.*beta*xi**2.*2.**(1./2.)*(A1)**(1./2.
     &)*log(x(2))+3.*A3*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b1*100.**(-
     &b2)*x(3)**b2*b3*beta**2.*x(1)**2.-2.*A3*x(4)*(A2)**(1./2.)*
     &x(2)**3.*rho*Cp**2.*b1*100.**(-b2)*x(3)**b2*xi*3.**(1./2.)*
     &2.**(1./2.)*(A1)**(1./2.)*log(x(2))-8.*g*alpha*Gr*Cp*rho*x(2
     &)**2.*x(1)**4.*beta*xi*2.**(1./2.)*(A1)**(1./2.)*log(D0)-6.
     &*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b0*beta**2.*x(1)**2.*xi*log(
     &x(2))+3.*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b0*beta**2.*x(1)**2.
     &*log(D0)-4.*g*alpha*Gr*Cp*rho*x(2)**2.*x(1)**4.*beta*xi**2.*
     &2.**(1./2.)*(A1)**(1./2.)*log(D0)+12.*A3*x(4)*T0*x(1)**2.*x
     &(2)**3.*rho*Cp**2.*b1*100.**(-b2)*x(3)**b2*b3*beta*2.**(1./
     &2.)*(A1)**(1./2.)-3.*A3*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b1*10
     &0.**(-b2)*x(3)**b2*beta**2.*x(1)**2.*xi**2.*log(x(2))+24.*x
     &(4)*T0*x(1)**2.*x(2)**3.*rho*Cp**2.*b0*beta*xi*2.**(1./2.)*
     &(A1)**(1./2.)*log(D0)+3.*A3*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b
     &1*100.**(-b2)*x(3)**b2*beta**2.*x(1)**2.*xi**2.*log(D0)+9.*
     &A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b1*100.**(-b2)*x(3)**b2*bet
     &a**3.*xi**2.*3.**(1./2.)*log(D0)+2.*A3*x(4)*(A2)**(1./2.)*x
     &(2)**3.*rho*Cp**2.*b1*100.**(-b2)*x(3)**b2*xi*3.**(1./2.)*2
     &.**(1./2.)*(A1)**(1./2.)*log(D0)-9.*A3*x(4)*T0**2.*x(1)**4.
     &*x(2)*Cp*b1*100.**(-b2)*x(3)**b2*beta**3.*xi*3.**(1./2.)*lo
     &g(x(2))-2.*A3*x(4)*(A2)**(1./2.)*x(2)**3.*rho*Cp**2.*b1*100
     &.**(-b2)*x(3)**b2*3.**(1./2.)*2.**(1./2.)*(A1)**(1./2.)*log
     &(x(2))-12.*x(4)*T0*x(1)**2.*x(2)**3.*rho*Cp**2.*b0*xi**2.*b
     &eta*2.**(1./2.)*(A1)**(1./2.)*log(x(2))-2.*2.**(1./2.)*x(4)
     &*(A2)**(1./2.)*x(2)**3.*rho*Cp**2.*b0*xi*3.**(1./2.)*(A1)**
     &(1./2.)*log(x(2))+3.*A3*x(4)*T0**2.*x(1)**4.*x(2)*Cp*b1*100
     &.**(-b2)*x(3)**b2*b3*xi**3.*beta**3.*3.**(1./2.)+9.*A3*x(4)
     &*T0**2.*x(1)**4.*x(2)*Cp*b1*100.**(-b2)*x(3)**b2*beta**3.*x
     &i*3.**(1./2.)*log(D0)+12.*A3*x(4)*T0*x(1)**2.*x(2)**3.*rho*
     &Cp**2.*b1*100.**(-b2)*x(3)**b2*beta*xi**2.*2.**(1./2.)*(A1)
     &**(1./2.)*log(D0)+6.*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b0*beta*
     &*2.*x(1)**2.*xi*log(D0)+2.*2.**(1./2.)*x(4)*(A2)**(1./2.)*x
     &(2)**3.*rho*Cp**2.*b0*xi*3.**(1./2.)*(A1)**(1./2.)*log(D0)-
     &3.*A3*x(4)*(A2)**(1./2.)*x(2)*Cp*T0*b1*100.**(-b2)*x(3)**b2
     &*beta**2.*x(1)**2.*log(x(2))-9.*A3*x(4)*T0**2.*x(1)**4.*x(2
     &)*Cp*b1*100.**(-b2)*x(3)**b2*beta**3.*xi**2.*3.**(1./2.)*lo
     &g(x(2))+24.*A3*x(4)*T0*x(1)**2.*x(2)**3.*rho*Cp**2.*b1*100.
     &**(-b2)*x(3)**b2*beta*xi*2.**(1./2.)*(A1)**(1./2.)*log(D0)-
     &24.*A3*x(4)*T0*x(1)**2.*x(2)**3.*rho*Cp**2.*b1*100.**(-b2)*
     &x(3)**b2*beta*xi*2.**(1./2.)*(A1)**(1./2.)*log(x(2)))


	f(3)=-3.*x(1)**2.*x(2)*T0*beta*Pi/(3.*beta*x(1)**2.*T0+3.*b
	&eta*x(1)**2.*T0*xi+3.**(1./2.)*(A2)**(1./2.))**2./(A1)**(1.
     &/2.)/(A2)**(1./2.)*(8.*A3*x(4)*x(2)**2.*T0*g*x(3)**(1.+b2)*
     &alpha*Gr*(A2)**(1./2.)*rho*Cp*b1*100.**(-b2)*b2*xi*3.**(1./
     &2.)*2.**(1./2.)+18*A3*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b1
     &*100.**(-b2)*x(3)**b2*beta**2.*3.**(1./2.)*xi**2.+4.*x(4)*x
     &(2)**2.*T0*g*x(3)*alpha*Gr*(A2)**(1./2.)*rho*Cp*b0*3.**(1./
     &2.)*2.**(1./2.)+8.*A3*x(4)*x(2)**2.*T0*g*x(3)**(1.+b2)*alph
     &a*Gr*(A2)**(1./2.)*rho*Cp*b1*100.**(-b2)*xi*3.**(1./2.)*2.*
     &*(1./2.)+6.*A3*x(4)*(A1)**(1./2.)*(A2)**(1./2.)*T0*b1*100.*
     &*(-b2)*x(3)**b2*b2*x(1)**2.*beta+6.*x(4)*(A1)**(1./2.)*(A2)
     &**(1./2.)*T0*b0*x(1)**2.*beta+12.*A3*x(4)*(A1)**(1./2.)*(A2
     &)**(1./2.)*T0*b1*100.**(-b2)*x(3)**b2*x(1)**2.*beta*xi+18*A
     &3*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b1*100.**(-b2)*x(3)**b
     &2*beta**2.*3.**(1./2.)*xi+18*x(4)*(A1)**(1./2.)*T0**2.*x(1)
     &**4.*b0*beta**2.*xi**2.*3.**(1./2.)+6.*A3*x(4)*(A1)**(1./2.
     &)*(A2)**(1./2.)*T0*b1*100.**(-b2)*x(3)**b2*x(1)**2.*beta+12
     &.*x(4)*(A1)**(1./2.)*(A2)**(1./2.)*T0*b0*x(1)**2.*beta*xi+6
     &.*A3*x(4)*(A1)**(1./2.)*(A2)**(1./2.)*T0*b1*100.**(-b2)*x(3
     &)**b2*b2*x(1)**2.*beta*xi**2.+72*A3*x(4)*x(2)**2.*T0**2.*g*
     &x(3)**(1.+b2)*alpha*Gr*beta*x(1)**2.*rho*Cp*b1*100.**(-b2)*
     &2.**(1./2.)*xi+6.*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b0*bet
     &a**2.*xi**3.*3.**(1./2.)+4.*x(4)*x(2)**2.*T0*g*x(3)*alpha*G
     &r*(A2)**(1./2.)*rho*Cp*b0*xi**2.*3.**(1./2.)*2.**(1./2.)+4.
     &*A3*x(4)*x(2)**2.*T0*g*x(3)**(1.+b2)*alpha*Gr*(A2)**(1./2.)
     &*rho*Cp*b1*100.**(-b2)*b2*3.**(1./2.)*2.**(1./2.)+72*A3*x(4
     &)*x(2)**2.*T0**2.*g*x(3)**(1.+b2)*alpha*Gr*beta*x(1)**2.*rh
     &o*Cp*b1*100.**(-b2)*xi**2.*2.**(1./2.)+24.*A3*x(4)*x(2)**2.
     &*T0**2.*g*x(3)**(1.+b2)*alpha*Gr*beta*x(1)**2.*rho*Cp*b1*10
     &0.**(-b2)*b2*2.**(1./2.)+24.*A3*x(4)*x(2)**2.*T0**2.*g*x(3)
     &**(1.+b2)*alpha*Gr*beta*x(1)**2.*rho*Cp*b1*100.**(-b2)*xi**
     &3.*2.**(1./2.)+6.*A3*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b1*
     &100.**(-b2)*x(3)**b2*beta**2.*3.**(1./2.)-g**2.*alpha**2.*G
     &r**2.*x(1)**2.*rho*x(2)*x(3)*(A2)**(1./2.)*3.**(1./2.)*2.**
     &(1./2.)+18*A3*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b1*100.**(
     &-b2)*x(3)**b2*b2*beta**2.*3.**(1./2.)*xi**2.+6.*x(4)*(A1)**
     &(1./2.)*(A2)**(1./2.)*T0*b0*x(1)**2.*beta*xi**2.+72*A3*x(4)
     &*x(2)**2.*T0**2.*g*x(3)**(1.+b2)*alpha*Gr*beta*x(1)**2.*rho
     &*Cp*b1*100.**(-b2)*b2*2.**(1./2.)*xi-2.*g**2.*alpha**2.*Gr*
     &*2.*x(1)**2.*rho*x(2)*x(3)*(A2)**(1./2.)*xi*3.**(1./2.)*2.*
     &*(1./2.)+6.*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b0*beta**2.*
     &3.**(1./2.)+18*A3*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b1*100
     &.**(-b2)*x(3)**b2*b2*beta**2.*3.**(1./2.)*xi+6.*A3*x(4)*(A1
     &)**(1./2.)*T0**2.*x(1)**4.*b1*100.**(-b2)*x(3)**b2*b2*beta*
     &*2.*3.**(1./2.)+8.*x(4)*x(2)**2.*T0*g*x(3)*alpha*Gr*(A2)**(
     &1./2.)*rho*Cp*b0*xi*3.**(1./2.)*2.**(1./2.)+6.*A3*x(4)*(A1)
     &**(1./2.)*T0**2.*x(1)**4.*b1*100.**(-b2)*x(3)**b2*beta**2.*
     &xi**3.*3.**(1./2.)+18*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b0
     &*beta**2.*xi*3.**(1./2.)+6.*A3*x(4)*(A1)**(1./2.)*(A2)**(1.
     &/2.)*T0*b1*100.**(-b2)*x(3)**b2*x(1)**2.*beta*xi**2.-g**2.*
     &alpha**2.*Gr**2.*x(1)**2.*rho*x(2)*x(3)*(A2)**(1./2.)*xi**2
     &.*3.**(1./2.)*2.**(1./2.)+4.*A3*x(4)*x(2)**2.*T0*g*x(3)**(1
     &.+b2)*alpha*Gr*(A2)**(1./2.)*rho*Cp*b1*100.**(-b2)*3.**(1./
     &2.)*2.**(1./2.)+4.*A3*x(4)*x(2)**2.*T0*g*x(3)**(1.+b2)*alph
     &a*Gr*(A2)**(1./2.)*rho*Cp*b1*100.**(-b2)*b2*xi**2.*3.**(1./
     &2.)*2.**(1./2.)+4.*A3*x(4)*x(2)**2.*T0*g*x(3)**(1.+b2)*alph
     &a*Gr*(A2)**(1./2.)*rho*Cp*b1*100.**(-b2)*xi**2.*3.**(1./2.)
     &*2.**(1./2.)+72*A3*x(4)*x(2)**2.*T0**2.*g*x(3)**(1.+b2)*alp
     &ha*Gr*beta*x(1)**2.*rho*Cp*b1*100.**(-b2)*b2*xi**2.*2.**(1.
     &/2.)+12.*A3*x(4)*(A1)**(1./2.)*(A2)**(1./2.)*T0*b1*100.**(-
     &b2)*x(3)**b2*b2*x(1)**2.*beta*xi+24.*A3*x(4)*x(2)**2.*T0**2
     &.*g*x(3)**(1.+b2)*alpha*Gr*beta*x(1)**2.*rho*Cp*b1*100.**(-
     &b2)*b2*xi**3.*2.**(1./2.)+24.*x(4)*x(2)**2.*T0**2.*g*x(3)*a
     &lpha*Gr*beta*x(1)**2.*rho*Cp*b0*2.**(1./2.)+24.*x(4)*x(2)**
     &2.*T0**2.*g*x(3)*alpha*Gr*beta*x(1)**2.*rho*Cp*b0*xi**3.*2.
     &**(1./2.)+72*x(4)*x(2)**2.*T0**2.*g*x(3)*alpha*Gr*beta*x(1)
     &**2.*rho*Cp*b0*xi*2.**(1./2.)+72*x(4)*x(2)**2.*T0**2.*g*x(3
     &)*alpha*Gr*beta*x(1)**2.*rho*Cp*b0*xi**2.*2.**(1./2.)+6.*A3
     &*x(4)*(A1)**(1./2.)*T0**2.*x(1)**4.*b1*100.**(-b2)*x(3)**b2
     &*b2*beta**2.*xi**3.*3.**(1./2.)+24.*A3*x(4)*x(2)**2.*T0**2.
     &*g*x(3)**(1.+b2)*alpha*Gr*beta*x(1)**2.*rho*Cp*b1*100.**(-b
     &2)*2.**(1./2.))

      f(4)=-1./8.*(x(1)**6.*Pi*eta*beta**2.*T0*xi*3.**(1./2.)*2.*
	&*(1./2.)*(A1)**(1./2.)+24.*P0*x(2)**2.*rho*Cp**2.*T0**2.*be
     &ta*x(1)**2.*xi+10.*x(1)**4.*Pi*eta*g*x(3)*alpha*Gr*T0*Cp*rh
     &o*x(2)**2.*xi*beta+x(1)**6.*Pi*eta*beta**2.*T0*3.**(1./2.)*
     &2.**(1./2.)*(A1)**(1./2.)+8.*P0*x(2)**2.*rho*Cp**2.*T0*3.**
     &(1./2.)*(A2)**(1./2.)+10.*x(1)**4.*Pi*eta*g*x(3)*alpha*Gr*T
     &0*Cp*rho*x(2)**2.*beta-2.*x(1)**2.*Pi*eta*g*x(3)*alpha*Gr*C
     &p*rho*x(2)**2.*3.**(1./2.)*(A2)**(1./2.)+24.*P0*x(2)**2.*rh
     &o*Cp**2.*T0**2.*beta*x(1)**2.-x(1)**4.*Pi*eta*(A2)**(1./2.)
     &*2.**(1./2.)*(A1)**(1./2.)*beta)/T0/eta/(3.*beta*x(1)**2.*T
     &0+3.*beta*x(1)**2.*T0*xi+3.**(1./2.)*(A2)**(1./2.))/Cp**2./
     &rho/x(2)**2.	

      return
      END


	SUBROUTINE newt(x,n,check)
      INTEGER n,nn,NP,MAXITS
      LOGICAL check
      REAL x(n),fvec,TOLF,TOLMIN,TOLX,STPMX
      PARAMETER (NP=40,MAXITS=200,TOLF=1.e-4,TOLMIN=1.e-6,TOLX=1.e-7,
     *STPMX=100.)
      COMMON /newtv/ fvec(NP),nn
      SAVE /newtv/
CU    USES fdjac,fmin,lnsrch,lubksb,ludcmp
      INTEGER i,its,j,indx(NP)
      REAL d,den,f,fold,stpmax,sum,temp,test,fjac(NP,NP),g(NP),p(NP),
     *xold(NP),fmin
      EXTERNAL fmin
      nn=n
      f=fmin(x)
      test=0.
      do 11 i=1,n
        if(abs(fvec(i)).gt.test)test=abs(fvec(i))
11    continue
      if(test.lt..01*TOLF)then
        check=.false.
        return
      endif
      sum=0.
      do 12 i=1,n
        sum=sum+x(i)**2
12    continue
      stpmax=STPMX*max(sqrt(sum),float(n))
      do 21 its=1,MAXITS !21
        call fdjac(n,x,fvec,NP,fjac)
        do 14 i=1,n
          sum=0.
          do 13 j=1,n
            sum=sum+fjac(j,i)*fvec(j)
13        continue
          g(i)=sum
14      continue
        do 15 i=1,n
          xold(i)=x(i)
15      continue
        fold=f
        do 16 i=1,n
          p(i)=-fvec(i)
16      continue
        call ludcmp(fjac,n,NP,indx,d)
        call lubksb(fjac,n,NP,indx,p)
        call lnsrch(n,xold,fold,g,p,x,f,stpmax,check,fmin)
        test=0.

        do 17 i=1,n
          if(abs(fvec(i)).gt.test)test=abs(fvec(i))
17      continue
        if(test.lt.TOLF)then
          check=.false.
          return
        endif
        if(check)then
          test=0.
          den=max(f,.5*n)
          do 18 i=1,n
            temp=abs(g(i))*max(abs(x(i)),1.)/den
            if(temp.gt.test)test=temp
18        continue
          if(test.lt.TOLMIN)then
            check=.true.
          else
            check=.false.
          endif
          return
        endif
        test=0.
        do 19 i=1,n
          temp=(abs(x(i)-xold(i)))/max(abs(x(i)),1.)
          if(temp.gt.test)test=temp
19      continue
        if(test.lt.TOLX)return

21    continue
      pause 'MAXITS exceeded in newt'
      END

	SUBROUTINE ludcmp(a,n,np,indx,d)
      INTEGER n,np,indx(n),NMAX
      REAL d,a(np,np),TINY
      PARAMETER (NMAX=500,TINY=1.0e-20)
      INTEGER i,imax,j,k
      REAL aamax,dum,sum,vv(NMAX)
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) pause 'singular matrix in ludcmp'
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n

          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)

          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      END

	SUBROUTINE lnsrch(n,xold,fold,g,p,x,f,stpmax,check,func)
      INTEGER n
      LOGICAL check
      REAL f,fold,stpmax,g(n),p(n),x(n),xold(n),func,ALF,TOLX
      PARAMETER (ALF=1.e-4,TOLX=1.e-7)
      EXTERNAL func
CU    USES func
      INTEGER i
      REAL a,alam,alam2,alamin,b,disc,f2,fold2,rhs1,rhs2,slope,sum,temp,
     *test,tmplam
      check=.false.
      sum=0.
      do 11 i=1,n
        sum=sum+p(i)*p(i)
11    continue
      sum=sqrt(sum)
      if(sum.gt.stpmax)then
        do 12 i=1,n
          p(i)=p(i)*stpmax/sum
12      continue
      endif
      slope=0.
      do 13 i=1,n
        slope=slope+g(i)*p(i)

13    continue
      test=0.
      do 14 i=1,n
        temp=abs(p(i))/max(abs(xold(i)),1.)
        if(temp.gt.test)test=temp
14    continue
      alamin=TOLX/test
      alam=1.
1     continue
        do 15 i=1,n
          x(i)=xold(i)+alam*p(i)
15      continue
        f=func(x)
        if(alam.lt.alamin)then
          do 16 i=1,n
            x(i)=xold(i)
16        continue
          check=.true.
          return
        else if(f.le.fold+ALF*alam*slope)then
          return
        else
          if(alam.eq.1.)then
            tmplam=-slope/(2.*(f-fold-slope))
          else
            rhs1=f-fold-alam*slope

            rhs2=f2-fold2-alam2*slope
            a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
            if(a.eq.0.)then
              tmplam=-slope/(2.*b)
            else
              disc=b*b-3.*a*slope
              if(disc.lt.0.) pause 'roundoff problem in lnsrch'
              tmplam=(-b+sqrt(disc))/(3.*a)
            endif
            if(tmplam.gt..5*alam)tmplam=.5*alam
          endif
        endif
        alam2=alam
        f2=f
        fold2=fold
        alam=max(tmplam,.1*alam)
      goto 1
      END

	SUBROUTINE lubksb(a,n,np,indx,b)
      INTEGER n,np,indx(n)
      REAL a(np,np),b(n)
      INTEGER i,ii,j,ll
      REAL sum
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
      END

	SUBROUTINE fdjac(n,x,fvec,np,df)
      INTEGER n,np,NMAX
      REAL df(np,np),fvec(n),x(n),EPS
      PARAMETER (NMAX=40,EPS=1.e-4)
CU    USES funcv
      INTEGER i,j
      REAL h,temp,f(NMAX)
      do 12 j=1,n
        temp=x(j)
        h=EPS*abs(temp)
        if(h.eq.0.)h=EPS
        x(j)=temp+h
        h=x(j)-temp
        call funcv(n,x,f)
        x(j)=temp
        do 11 i=1,n
          df(i,j)=(f(i)-fvec(i))/h
11      continue
12    continue
      return
      END

      FUNCTION fmin(x)
      INTEGER n,NP
      REAL fmin,x(*),fvec
      PARAMETER (NP=40)
      COMMON /newtv/ fvec(NP),n
      SAVE /newtv/
CU    USES funcv
      INTEGER i
      REAL sum
      call funcv(n,x,fvec)
      sum=0.
      do 11 i=1,n
        sum=sum+fvec(i)**2
11    continue
      fmin=0.5*sum
      return
      END