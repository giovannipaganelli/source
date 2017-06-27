     H/TITLE Driver stampa blocchi acquisti SITREP
     H Option(*srcstmt:*NoDebugIo)
     ***************************************************************
      * Programma      :AF607
      * Descrizione    :Driver stampa blocchi acquisti SITREP
      * Programmatore  :Andrea Somaini
      * Data           :26/04/2016
     ***************************************************************
      * Modifiche      :.....
     ***************************************************************
      *Definizione files
     FAF607FM   cf   e             Workstn INFDS(WSFBA)

      *INFDS DELLA DSPF:
     D WSFBA           DS
     D  @WSREC                38     45
     D  $tasto               369    369
     D  @WSCUR               370    371B 0
     D  @WSSFR               378    379B 0
     * Ds di sistema
     D                sds
     D  @Pgm                   1     10                                         Programa
     D  @NJob                264    269  0                                      Control Errores

      *Definizione interfaccia
     D AF607           pr
     D  I_Kpjba                            LikeDs(Kpjba)

     D AF607           pi
     D  I_Kpjba                            LikeDs(Kpjba)

     * DS esterne
     D Kpjba         e ds
     D ##udb         e ds                  Dtaara
     D Zaf607        e ds

     * Campi di comodo
     D Des15           s             15a
     D IfErroreC1      s               n

     * Ds lettura tabelle
     D DsTab           ds                  qualified
     D  IfFound                        n
     D  Data                        150a

     * Tasto ricerca tabella
     D I@Cel1          pr                  extpgm('I@CEL1')
     D  a                             3a   Const
     D  b                             1a
     D  c                            35a
     D  d                            15a
     D  e                             1a   Const
     D  f                                  Like(##Clin)
     D  g                                  Like(##Cfam)

     * Tasto ricerca UDB
     D I@CUDB          pr                  extpgm('I@CUDB')
     D  Udb                           3a   Const
     D  DescrUdb                     15a   Const

       //*-------------------------------------------------------------*
       //*-   MAIN                                                    -*
       //*-------------------------------------------------------------*
       Kpjba = I_Kpjba ;
       in ##udb;

       @C1#UDB = ##cudb;

       //Gestione videata
       DoU *InKc;
         Exfmt C1;

         Exsr Ctrl_C1;

         //F4 - Ricerca
         If *inkd;
           Exsr Ricerca;
         EndIf;

         //F5 - Elabora
         If *inke and Not IfErroreC1;
           @MsgerC1 = 'SNS4025';
           Exsr Elabora;
         EndIf;
       EndDo;

       *InLr = *on;
       Return;

      *------------------------------------------------------------
      *Controlli videata
      *------------------------------------------------------------
       Begsr Ctrl_C1;

        //Stagione
        *in98 = *off;
        WC1CSTGD = '';
        If WC1CSTG <> '';
          DsTab = LeggiTabella('STG': WC1CSTG);
          If DsTab.IfFound = *off;
            SetError(*in98:'SNS1046');
          Else;
            WC1CSTGD = %subst(DsTab.Data:1:35);
          EndIf;
        EndIf;

        //Famiglia
        *in97 = *off;
        WC1CFAMD = '';
        If WC1CFAM <> '';
          DsTab = LeggiTabella('FAM': WC1CFAM);
          If DsTab.IfFound = *off;
            SetError(*in97:'SNS1022');
          Else;
            WC1CFAMD = %subst(DsTab.Data:1:35);
          EndIf;
        EndIf;

        //UDB
        *in96 = *off;
        WC1UDBFD = '';
        If WC1UDBF <> '';
          Exec Sql
            Select APDESC
              Into :WC1UDBFD
              From UDBANAPF
              Where APCUDB = :WC1UDBF
                And A001AP = '';

          If SqlCode <> 0;
            SetError(*in96:'SNS1267');
          EndIf;
        EndIf;
       EndSr;

      *------------------------------------------------------------
      *Comando ricerca
      *------------------------------------------------------------
       Begsr Ricerca;

         //Stagione
         If C1Cam = 'WC1CSTG';
           I@Cel1 ('STG'        :
                   WC1CSTG      :
                   WC1CSTGD     :
                   DES15        :
                   'C'          :
                   ##Clin       :
                   ##CFam       ) ;
         EndIf;

         //Famiglia
         If C1Cam = 'WC1CFAM';
           I@Cel1 ('FAM'        :
                   WC1CFAM      :
                   WC1CFAMD     :
                   DES15        :
                   'C'          :
                   ##Clin       :
                   WC1CFAM      ) ;
         EndIf;

         //UDB
         If C1Cam = 'WC1UDBF';
           I@CUDB (WC1UDBF     :
                   WC1UDBFD);
         EndIf;

       EndSr;

      *------------------------------------------------------------
      *Lancio elaborazione in batch
      *------------------------------------------------------------
       Begsr Elabora;

         Clear Zaf607;
         CSTGCK = WC1CSTG;
         CFAMCK = WC1CFAM;
         CUDBCK = WC1UDBF;

         KPJBU = Zaf607;
         KCOAZ = 'A608';

         //BCH10(Kpjba);
     c                   call      'BCH10'
     c                   parm                    kpjba
     c*                  call      'AF608'
     c*                  parm                    kpjba
       Endsr;

      *------------------------------------------------------------
      *Accende errore in videata di dettaglio
      *------------------------------------------------------------
     p SetError        b
     d SetError        pi
     d   Indicatore                    n
     d   Messaggio                    8a   const
       IfErroreC1 = *On ;
       Indicatore = *On ;
       If @MSGERC1 = '';
         @MSGERC1 = Messaggio ;
       EndIf;
     p SetError        e

      *------------------------------------------------------------
      *LeggiTabella
      *------------------------------------------------------------
     p LeggiTabella    b
     d LeggiTabella    pi                  likeds(DsTab)
     d   CodTabella                   3a   const
     d   KeyTabella                   6a   const

     d CodTab          s                   like(CodTabella)
     D DataTab         s            150a
     d Errore          s              1a
     d KeyTab          s                   like(KeyTabella)
     d Ret             ds                  likeds(DsTab)

     d @TAB            pr                  ExtPgm('@TAB')
     d  CodTabella                    3a
     d  KeyTabella                    6a
     d  Dettaglio                   150a
     d  Errore                        1a
     d  Lingua                        1a
     d  Famiglia                      1a

      /Free
       Clear Ret;
       CodTab = CodTabella;
       KeyTab = KeyTabella;
       @TAB(CodTab    :
            KeyTab    :
            DataTab   :
            Errore    :
            ##clin    :
            ##cfam    );

       If Errore <> '';
         Return Ret;
       EndIf;

       Ret.IfFound = *on;
       Ret.Data = DataTab;
       Return Ret;

      /End-Free
     p                 e
