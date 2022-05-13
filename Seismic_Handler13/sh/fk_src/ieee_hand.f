      integer function myhandler(sig, code, context, addr)
c
c     datei ieee_hand(ler).f    F.Roth <900808.1645 ?
c
c     To trap on common exceptions and continue processing
c
c     Insert in your main pgm :
cm    external myhandler                                                sun warn
cm    ieeer=ieee_handler("set","all",myhandler)                         sun warn
cm    if(ieeer.ne.0) write(ltyo,'('' % Could not establish fp signal hansun warn
cm   *dler !'')')                                                       sun warn
c
c     calls     :
c     entries   : --
c     input from : --
c     output to  : standard error
c
      integer sig,code,context(5),addr
c
c     Use loc(code) and loc(addr) to refer to these parameters
c
2000  format(' Hier ist IEEE-exceptions handler')
2077  format('ieee exception code ',Z3,' occurred at pc ',Z8)
2078  format(' SunOs 4.x codes for ieee signals :'/
     1       ' invalid operation = d0'/
     2       ' devide by zero    = c8'/
     3       ' overflow          = d4'/
     4       ' underflow         = cc'/
     5       ' inexact           = c4')
c
c---------------------------------------------------------
c
ct    write(*,2000)
      write(0,2077) loc(code), loc(addr)
ct    write(0,2078)
c
c
      return
      end
