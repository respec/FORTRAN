Module WdmEntDriver

    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Function F90_WDMOPN(ByRef aWdmsFile As Integer, ByVal aSourceFile As String, ByVal aSourceFileNameLength As Short) As Integer
    End Function
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Function F90_WDMCLO(ByRef aWdmsFile As Integer) As Integer
    End Function
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Function F90_WDBOPN(ByRef aFileUnit As Integer, ByVal aSourceFile As String, ByVal aSourceFileNameLength As Short) As Integer
    End Function
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Function F90_WDCKDT(ByRef aWdmsFile As Integer, ByRef aDSN As Integer) As Integer
    End Function
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Function F90_WDFLCL(ByRef aWdmsFile As Integer) As Integer
    End Function
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDDSNX(ByRef aWdmsFile As Integer, ByRef aDsn As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WTFNDT(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aGpflg As Integer, ByRef aTdsfrc As Integer, ByVal aSDate() As Integer, ByVal aEDate() As Integer, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDTGET(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aDelt As Integer, ByVal aDates() As Integer, _
                          ByRef aNval As Integer, ByRef aDtran As Integer, ByRef aQualfg As Integer, ByRef aTunits As Integer, ByVal aRval() As Single, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDTPUT(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aDelt As Integer, ByVal aDates() As Integer, _
                          ByRef aNval As Integer, ByRef aOvfg As Integer, ByRef aQualfg As Integer, ByRef aTunits As Integer, ByVal aRval() As Single, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDBSGI(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aSaind As Integer, ByRef aSalen As Integer, _
                          ByRef aSaval As Integer, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDBSAI(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aMsFile As Integer, ByRef aSaind As Integer, ByRef aSalen As Integer, _
                          ByRef aSaval As Integer, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDBSGR(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aSaind As Integer, ByRef aSalen As Integer, _
                          ByRef aSaval As Single, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDBSAR(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aMsFile As Integer, ByRef aSaind As Integer, ByRef aSalen As Integer, _
                          ByRef aSaval As Single, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDBSGC(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aSaind As Integer, ByRef aSalen As Integer, _
                          ByVal aSaval() As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDBSGCX(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aSaind As Integer, ByRef aSalen As Integer, _
                           ByVal aSaval As String)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)> _
    Public Sub F90_WDBSAC(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aMsFile As Integer, ByRef aSaind As Integer, ByRef aSalen As Integer, _
                          ByRef aRetcod As Integer, ByRef aSaval As String, ByVal aStringLength As Short)
    End Sub

    Dim gOutFileStream As IO.FileStream

    Sub Main()

        'local(variables)
        Dim WDMSFL, MESSFL, RETCOD, DSN, DSTYPE, I As Integer
        Dim WDNAME, WDMNAMES(3), MSNAME As String
        Dim lOutStream() As Byte

        Debug.Print("Start WdmEntDriverFortran")

        If (Find_Test_Folder()) Then
            WDMNAMES(1) = "test.wdm"
            WDMNAMES(2) = "missing.wdm"
            WDMNAMES(3) = "corrupt.wdm"

            For I = 1 To 1 'not testing 2 and 3 for now
                WDNAME = WDMNAMES(I)
                lOutStream = (System.Text.Encoding.Unicode.GetBytes("Testing " & WDNAME & vbCrLf))
                gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                WDMSFL = 40
                RETCOD = F90_WDMOPN(WDMSFL, WDNAME, Len(WDNAME))
                lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDMOPN Return Code " & RETCOD & " Opening " & Trim(WDNAME) & vbCrLf))
                gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                If (RETCOD >= 0) Then
                    RETCOD = F90_WDMCLO(WDMSFL)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDMCLO Return Code " & RETCOD & " Closing " & WDMSFL & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                End If

                WDMSFL = F90_WDBOPN(0, WDNAME, Len(WDNAME))
                If (WDMSFL <= 0) Then
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBOPN Return Code " & WDMSFL & " Opening " & Trim(WDNAME) & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                Else
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBOPN Return Code " & WDMSFL & " Opening " & Trim(WDNAME) & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                    DSN = 39
                    DSTYPE = F90_WDCKDT(WDMSFL, DSN)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDCKDT: DSN, TYPE: " & DSN & ", " & DSTYPE & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    DSN += 1
                    F90_WDDSNX(WDMSFL, DSN)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDDSNX: DSN: " & DSN & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    Dim lGpFlg As Integer = 1
                    Dim lDsfrc As Integer
                    Dim lSdat(6) As Integer
                    Dim lEdat(6) As Integer
                    F90_WTFNDT(WDMSFL, DSN, lGpFlg, lDsfrc, lSdat, lEdat, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WTFNDT: DSN, SDAT, EDAT: " & DSN & ", " & lSdat(0).ToString & ", " & lEdat(0).ToString & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    Dim lDates(5) As Integer
                    Dim lDelt As Integer = 1
                    Dim lNvals As Integer = 10
                    Dim lTran As Integer = 0
                    Dim lQual As Integer = 31
                    Dim lTunit As Integer = 4
                    lDates(0) = 1976 : lDates(1) = 4 : lDates(2) = 5 : lDates(3) = 24 : lDates(4) = 0 : lDates(5) = 0
                    Dim lRval(10) As Single
                    F90_WDTGET(WDMSFL, DSN, lDelt, lDates, lNvals, lTran, lQual, lTunit, lRval, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDTGET: DSN, RVAL: " & DSN & ", " & lRval(0).ToString & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    Dim lOvfg As Integer = 1
                    lRval(1) = 1.0
                    lRval(2) = 2.0
                    lRval(3) = 3.0
                    F90_WDTPUT(WDMSFL, DSN, lDelt, lDates, lNvals, lOvfg, lQual, lTunit, lRval, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDTPUT: DSN, RETCOD: " & DSN & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    'work with attributes
                    'have to open message wdm first
                    MSNAME = "e:\respec\FORTRAN\lib3.0\hspfmsg.wdm"
                    MESSFL = F90_WDBOPN(1, MSNAME, Len(MSNAME))

                    'tsgroup
                    Dim lSaind As Integer = 34
                    Dim lSalen As Integer = 1
                    Dim lSaval As Integer
                    F90_WDBSGI(WDMSFL, DSN, lSaind, lSalen, lSaval, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBSGI: SAVAL, RETCOD: " & lSaval & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    F90_WDBSAI(WDMSFL, DSN, MESSFL, lSaind, lSalen, lSaval, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBSAI: SAVAL, RETCOD: " & lSaval & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    'elev
                    lSaind = 7
                    lSalen = 1
                    Dim lRsaval As Single
                    F90_WDBSGR(WDMSFL, DSN, lSaind, lSalen, lRsaval, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBSGR: SAVAL, RETCOD: " & lRsaval & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    lRsaval = 111.2
                    F90_WDBSAR(WDMSFL, DSN, MESSFL, lSaind, lSalen, lRsaval, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBSAR: SAVAL, RETCOD: " & lRsaval & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    'stanam
                    lSaind = 45
                    lSalen = 48
                    Dim lIval(80) As Integer
                    F90_WDBSGC(WDMSFL, DSN, lSaind, lSalen, lIval)
                    'need to turn these integer values back into a character string
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBSGC: IVAL, RETCOD: " & lIval(1) & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    Dim lCval As String = ""
                    F90_WDBSGCX(WDMSFL, DSN, lSaind, lSalen, lCval)
                    'need to turn these integer values back into a character string
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBSGC: IVAL, RETCOD: " & lCval & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    lCval = "Test Stanam"
                    F90_WDBSAC(WDMSFL, DSN, MESSFL, lSaind, lSalen, RETCOD, lCval, Len(lCval))
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBSAC: CVAL, RETCOD: " & lCval & ", " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    'leave open for multiple wdm test
                    'RETCOD = F90_WDFLCL(WDMSFL)
                    'lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDFLCL Return Code " & RETCOD & vbCrLf))
                    'gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                End If

                'TBW specific tests
                Dim lDsns() As Integer = {9022, 231}
                WDNAME = "divrsion.wdm"
                lOutStream = (System.Text.Encoding.Unicode.GetBytes("Testing " & WDNAME & vbCrLf))
                gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                WDMSFL = F90_WDBOPN(0, WDNAME, WDNAME.Length)
                If (WDMSFL < 0) Then
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBOPN Return Code " & WDMSFL & " Opening " & Trim(WDNAME) & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                Else
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBOPN Return Code " & WDMSFL & " Opening " & Trim(WDNAME) & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                    For Each DSN In lDsns
                        DSTYPE = F90_WDCKDT(WDMSFL, DSN)
                        lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDCKDT: DSN, TYPE: " & DSN & ", " & DSTYPE & vbCrLf))
                        gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                        Dim lDelt As Integer = 1
                        Dim lNvals As Integer = 10
                        Dim lTran As Integer = 0
                        Dim lQual As Integer = 1
                        Dim lTunit As Integer = 4

                        Dim lDates() As Integer = {1988, 1, 1, 0, 0, 0}
                        Dim lRval(10) As Single
                        Call F90_WDTGET(WDMSFL, DSN, lDelt, lDates, lNvals, lTran, lQual, lTunit, lRval, RETCOD)
                        lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDTGET1: DSN, RETCOD, RVAL: " & DSN & ", " & RETCOD & ", " & lRval(0).ToString & vbCrLf))
                        gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                        Dim lRSAVAL As Single = lRval(0)

                        Dim lOvfg As Integer = 1
                        lRval(0) = -2.97520661
                        Call F90_WDTPUT(WDMSFL, DSN, lDelt, lDates, lNvals, lOVFG, lQual, lTunit, lRval, RETCOD)
                        lOutStream = (System.Text.Encoding.Unicode.GetBytes("   F90_WDTPUT1: DSN, RETCOD: " & DSN & ", " & RETCOD & vbCrLf))
                        gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                        Call F90_WDTGET(WDMSFL, DSN, lDelt, lDates, lNvals, lTran, lQual, lTunit, lRval, RETCOD)
                        lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDTGET2: DSN, RETCOD, RVAL: " & DSN & ", " & RETCOD & ", " & lRval(0).ToString & vbCrLf))
                        gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                        lRval(0) = lRSAVAL
                        Call F90_WDTPUT(WDMSFL, DSN, lDelt, lDates, lNvals, lOvfg, lQual, lTunit, lRval, RETCOD)
                        lOutStream = (System.Text.Encoding.Unicode.GetBytes("   F90_WDTPUT2: DSN, RETCOD: " & DSN & ", " & RETCOD & vbCrLf))
                        gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                        Call F90_WDTGET(WDMSFL, DSN, lDelt, lDates, lNvals, lTran, lQual, lTunit, lRval, RETCOD)
                        lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDTGET3: DSN, RETCOD, RVAL: " & DSN & ", " & RETCOD & ", " & lRval(0).ToString & vbCrLf))
                        gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                    Next
                End If

                gOutFileStream.Flush()
            Next I
        Else
            Debug.Print("Test Folder Not Found")
        End If

        gOutFileStream.Close()
        Debug.Print("End WdmEntDriverVB")

    End Sub

    Function Find_Test_Folder() As Boolean
        Try
            Dim Cdir As String = CurDir()
            Debug.Print("Base directory Is:       " & Cdir)
            Dim BaseFolderParts() As String = Cdir.Split("\")
            Dim l As Integer = UBound(BaseFolderParts)
            Dim ResultsFileName As String = BaseFolderParts(l - 1) & "!" & BaseFolderParts(l) & "!Results.out"
            Cdir &= "\..\..\..\test"
            ChDir(Cdir)
            Debug.Print("Test directory is: " & CurDir())
            If IO.File.Exists(ResultsFileName) Then
                IO.File.Delete(ResultsFileName)
            End If
            gOutFileStream = IO.File.Open(ResultsFileName, IO.FileMode.Create)
            Dim lOutStream() As Byte = System.Text.Encoding.Unicode.GetBytes("Test directory is: " & CurDir() & vbCrLf)
            gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
            Find_Test_Folder = True
        Catch
            Debug.Print("Failed to Set Test directory ")
            Find_Test_Folder = False
        End Try
    End Function

End Module
