; Program for orbital elements calculation - Have fun!
$ Prepared by A. Emran, PhD Student in Space and Planetary Sciences, University of Arkansas.
$ Email: alemran@uark.edu; Ph: (334)-400-9371.

PRO Orbital_mechanics

 ;#DESIGN OF THE TRANSFER ELLIPSE (FIG. 3.56; PAGE 121)
 ; Let, the radius of earth and Mars are r1 and r2, respectively
  r1=149905909.7 ; in km
  r2=206671197.0 ; in km
  
 ; Given that the longitude of Earth (Lo) at launch and Mars (Lm)at arrival
  Lo=181.444 ; celestial longitude in degree
  Lm=333.221 ; celestial longitude in degree
  
 ; Let, the line of apside = x ; (in degree)
  $ true anomaly at Earth is ta_e and Mars is ta_m
  x=161.714 ; in degree
  print, "Line of apsides", x
  ta_e=abs(Lo-x) ; true anomaly at Earth (absolute value in degree)
    IF (ta_e GT 180) THEN ta_e=((360-Lo)+x) ELSE BEGIN
      ta_e=abs(Lo-x)
      ENDELSE
  ta_m=abs(Lm-x) ; true anomaly at Mars (absolute value in degree)
    IF (ta_m GT 180) THEN ta_m=((360-Lm)+x) ELSE BEGIN
      ta_m=abs(Lm-x)
      ENDELSE
    Print, "True anomaly at departure and arrival", ta_e, ta_m ;return the value of true anomaly at Earth and Mars
      
 ; Consider, the ecentricity of transfer ecllipse is e (unit less)
  e=(r2-r1)/((r1*cos(!dtor*ta_e))-(r2*cos(!dtor*ta_m))) ; system variables !dtor and !radeg convert between rad and deg
    print, "Ecentricity", e ; return the value of e
 
 ; Find the radius of perapsis = rp (in km)
  rp= (r1*(1+e*cos(!dtor*ta_e)))/(1+e)
    print, "Radius of periapsis", rp ; return the value of rp
    
 ; Find semi-major axis = a of the transfer ellipse
  a=rp/(1-e); in km
    print, "Radious of Semi-major axis", a ; return the value of a
  
 ; Find the radius of apoapsis = ra (in km)
  ra= a*(1+e) ; in km
    print, "Radius of apoapsis", ra ; return the value of ra
    
 ; Mean motion of the transfer ellipse= n, where the gravitional parameter of Sun =s_gp (in km)
  s_gp=132712439935.5; in km
  n=sqrt(s_gp/(a^3))
    print, "Mean motion of the transfer ellipse", n ; return the value of n
    
 ; Calculate the ecentricity anomay for Earth and Mars are Ee and Em, respectively (in radian)
  Ee=acos((e+cos(!dtor*ta_e))/(1+(e*cos(!dtor*ta_e)))) ; acos is the inverse of cosine angle in radian
  Em=acos((e+cos(!dtor*ta_m))/(1+(e*cos(!dtor*ta_m)))) ; acos is the inverse of cosine angle in radian
    print, "Ecentricity anomay for Earth and Mars", Ee, Em ; return the values of Ee and Em
    
 ; Find the time since periapsis at departure (Earth) and arrival (Mars) are td and ta, respectively
  td=(Ee-(e*sin(Ee)))/n ; in seconds
  ta=(Em-(e*sin(Em)))/n ; in seconds
    print, "Time since periapsis at departure in second (Earth) and arrival (Mars)", td, ta ; return the values of td and ta
    
 ; Finally, calculate the flight time Ft in days (from departure to arrrival)
  Ft=ta-td ; in Seconds
  Ft_days=Ft/86400 ; in days (covert seconds to days by; divide the time value by 86400)
  Ft_years=Ft_days/365 ; in years (covert days to years; by divide the time value by 365)
    print, "Flight time in seconds, days, and year (from departure to arrrival)", Ft, Ft_days, Ft_years ;return the values of Ft, Ft_days, and Ft_years
    
 ; Velocity of transfer ellipse at departure and arrival are Vd and Va, respectively
  Vd=sqrt(((2*s_gp)/r1)-(s_gp/a)) ; in km/sec
  Va=sqrt(((2*s_gp)/r2)-(s_gp/a)) ; in km/sec
    print, "Velocity of transfer ellipse at departure and arrival", vd, va ; return the velocity of transfer ellipse
    
 ; Flight path angle of transfer ellipse at departure and arrival are FPAd and FPAa, respectively
   FPAd=180/!PI*atan((e*sin(!dtor*ta_e))/(1+(e*cos(!dtor*ta_e))))
   FPAa=180/!PI*atan((e*sin(!dtor*ta_m))/(1+(e*cos(!dtor*ta_m))))
     print, "Flight path angle of transfer ellipse at departure and arrival", FPAd, FPAa
 
 ;#DESIGN OF THE DEPARTURE TRAJECTORY 
 $ PLANE CHANGE SECTION (FIG. 3.57; PAGE 123) 
 ; Let consider the following, it= the inclination of transfer plane (in degree) in respect to ecliptic plane and
  $ a1, b1, c1 are the sides of sphereical triangle at departure trajectory
  im=1.8497 ; the inclination of Mars orbital plane
  aa=(180-im) ; angle in speherical triagle 
  anm=49.572 ; Mars longitude of ascending node
  a1=(anm+180)-Lo ; sides of sphereical triangle at departure trajectory
  b1=Lm-(anm+180) ; sides of sphereical triangle at departure trajectory
  c1=180/!PI*acos((cos(!dtor*a1)*cos(!dtor*b1))+(sin(!dtor*a1)*sin(!dtor*b1)*cos(!dtor*aa))) ; sides of sphereical triangle at departure trajectory
  it=180/!PI*asin((sin(!dtor*aa)*sin(!dtor*b1))/sin(!dtor*c1)) ; in degree
    print, "The inclination of transfer plane", it, aa, a1, b1, c1
  
 ;$ CALCULATING VHE AND C3 SECTION (FIG. 3.57 AND 3.58; PAGE 124-125)      
 ; Given that the orbital valocity of Earth and Mars at the departure and arrival are Ve/s and Vm/s
  $ FPAd= The flight path angle of the spacecraft on the transfer ellipse
  $ ab= the angle alpha in the Fig. 3.59
  Ves=29.892 ; Velocity of Earth with respect to the Sun (in km/sec)
  Vms=26.4964 ; Valocity of Mars with respect to the Sun (in km/sec)
  Vss=Vd ; velocity of spacecraft on transfer ellipse at departure (Earth position)
  FPAearth=0.93486 ; flight path angle of Earth in degree (given data)
  ab=180/!PI*acos(cos(!dtor*it)*cos(!dtor*(FPAd-FPAearth))) ; rememeber the NOTE on Page 125 and see the equation 3.137
  $ Note: if FPAd and FPAearth have the same sign they should be subtracted otherwise if the signs of FPAd and FPAearth differ then
  $they should be added (for more info. pls consult  Page 125 and see the equation 3.137)
  C3=(Ves^2+Vss^2-(2*Ves*Vss*cos(!dtor*ab))); see the equation 3.138
  VHE=sqrt(C3) ; excess velocity
     print,  "Alpha, C3 and VHE", ab, C3, VHE
     
 ;# Inclination of transfer ellipse relative to Mars orbit
  ; Given the alpha, a1, and c1 are same
  ; itp= the inclination of transfer ellise relative to Mars orbiral plane
  itp=180/!PI*asin((sin(!dtor*aa)*sin(!dtor*a1))/sin(!dtor*c1)) ; in degree
    print, "The inclination of transfer ellipse relative to Mars orbit", itp
    
 ;# The Velocity at infinity on the arrival hyperbola
  $ aba= the angle alpha in the Equ. 3.140 (in degree)
  $ VEM = velocity at infinity on the arrival hyperbola
  $ Vms=26.4964 ; Valocity of Mars (target planet) with respect to the Sun (in km/sec
  $ Vsa=Va ; velocity of spacecraft on transfer ellipse at arrival (Mars position)
  $ FPAa = flight patha angle of spacecraft on the transfer ellispe at arrival
  $ itp= the inclination of transfer ellise relative to Mars orbiral plane
  FPAmars= -0.24524 ; flight path angle of Mars in degree (given data)
  aba=180/!PI*acos(cos(!dtor*itp)*cos(!dtor*(FPAa+FPAmars))) ; in degree
  $ Note: if FPAd and FPAearth have the same sign they should be subtracted otherwise if the signs of FPAd and FPAearth differ then
  $they should be added (for more info. pls consult  Page 125 and see the equation 3.137)
  C_3=(Vms^2+Va^2-(2*Vms*Va*cos(!dtor*aba))) ; C3 value for calculating 
  VEM=sqrt(C_3) ; velocity at infinity on the arrival hyperbola velocity at infinity on the arrival hyperbola
    print, "Alpha, C_3, and velocity at infinity on the arrival hyperbola", aba, C_3, VEM
    
  ;# Elements of arrival hyperbola
   $ Vp=the velocity at periapsis of the approach hyperbola
   $ a_hy = Semi-majoraxis of the approach hyperbola
   $ b_hy = Semi-minor axis of the approach hyperbola
   $ e_hy = ecentricty the approach hyperbola
   $ beta_hy = angle of asymtote
   rp= (3397+220); radius
   meu_mars= 42828.3 ; Gravitational constant for Mars
   vp=sqrt((VEM^2)+(2*meu_mars)/rp) ; km/s using eq. 3.141
   a_hy = meu_mars/(VEM^2) ; in km using eq. 3.78
   e_hy = 1+(rp/a_hy) ; using eq. 3.67ek
   b_hy = a_hy*sqrt((e_hy^2)-1) ; in km using eq. 3.82
   beta_hy = 180/!PI*atan(b_hy/a_hy) ; using the equation 3.63
    print, "Velocity at periapsis, ecenticity, semi-major, semi-minor, and angle of asymptote of the approach hyperbola", vp, e_hy, a_hy, b_hy, beta_hy
     
  ;# Velocity change required for to establish a circular orbit at 220 Km
  $ v_cr= velocity of the final circular orbit
  $ v_req = velocity required to establish the circular orbit
  v_cr = sqrt(meu_mars/rp); in km using eq. 3.6
  v_req = vp-v_cr ; delta v
    print, "Velocity of circular orbit and velocity change required", v_cr, v_req
      
  ;# On-orbit dry mass
  ;Exterpolation of the figure 2.7 (page 24) the on-orbit dry mass is for a  Phase-A estimated sciences payload mass of 400 kg
  payload = 400 ; in kg
  dry_mass = (payload+6.7333)/0.1299
    print, "On-orbit dry mass", dry_mass
      
  ;# Propulsion system and propultion mass
  ;Mono-propellant system
  ;mp = Propellant mass required
  Isp = 259 ; (in sec) since we are using Mono-propellant system
  g = 9.81 ; gravity of earth
  mf = dry_mass ; this is final mass or dry mass
  mp= mf*(exp((v_req*1000)/(g*Isp))-1)
  margin = mp*.1 ; 10 percent margin
  total = mp+margin
    print, "Propellent mass required and margin", mp, margin, total
          
 ; DONE 
 END
