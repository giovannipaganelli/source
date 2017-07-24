     H/TITLE Associazione matricola a fascetta cravatta
     H decedit('0,') datedit(*ymd/)
     H bnddir('MODSNSILE')
     H option(*srcstmt:*nodebugio)
     H optimize(*NONE)
     *---------------------------------------------------------------*
     *           E R M E N E G I L D O   Z E G N A                   *
     *                                                               *
     * Azienda       : CONSITEX SA                                   *
     * Programma     : BEEP                                          *
     * Descrizione   : Prova beep su window                          *
     *                                                               *
     *                                                               *
     *  (®) E.Z. Divisione Formale    MODIFICATOOOOO  f  . ggg     .    *
     *
     *  - Sistemi Informativi -                                      *
     *---------------------------------------------------------------*
     FBEEPFM    cf   e             workstn INFDS(WSFBA)
     *---------------------------------------------------------------*
      *INFDS DELLA DSPF:
     D WSFBA           DS
     D  @WSREC                38     45                                         Nome record
     D  @WSAID               369    369                                         Help attivato
     D  @WSCUR               370    371B 0                                      Posizione cursore
     D  @WSSFR               378    379B 0                                      N[reg subfile attivo
     *
    D PGMDS          SDS
     D  @pgm                   1     10
     D  @user                254    263
     D  @sds81                81    269
     D  @njob                264    269  0
     D  @parms           *parms
     D  @nsen                227    232  0
     D  @stat            *status
     D  @sds                   1    253
     *
     D TitWin          C                   CONST(' Errore Barcode Matricola ')
     D j               s              5s 0 inz
     D max             s              5s 0 inz (3)
     D DlySeconds      s              5s 0 inz(1)
     D sleep           PR            10U 0 extproc('sleep')
     D   seconds                     10U 0 value
     * ------------------------------------------------------------
     * --- M A I N
     * ------------------------------------------------------------
     *
     c                   write     w1
     c                   write     w1
      /free
         eval Title = TitWin ;

             For j = 1 to max ;
              Write w1;
              write w1;
          //  Callp sleep(DlySeconds);
          // Modificao e GIT example
             Endfor;
         Exfmt  w1;

         eval *inlr=*on ;
      /end-free
     * ----------------------------------------------
