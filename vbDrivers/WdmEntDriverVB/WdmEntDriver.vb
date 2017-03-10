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
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)>
    Public Function F90_WDFLCL(ByRef aWdmsFile As Integer) As Integer
    End Function
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)>
    Public Sub F90_WDDSNX(ByRef aWdmsFile As Integer, ByRef aDsn As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)>
    Public Sub F90_WDTGET(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aDelt As Integer, ByVal aDates() As Integer,
                          ByRef aNval As Integer, ByRef aDtran As Integer, ByRef aQualfg As Integer, ByRef aTunits As Integer, ByVal aRval() As Single, ByRef aRetcod As Integer)
    End Sub
    <DllImport("WdmEnt.dll", CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)>
    Public Sub F90_WTFNDT(ByRef aWdmsFile As Integer, ByRef aDsn As Integer, ByRef aGpflg As Integer, ByRef aTdsfrc As Integer, ByVal aSDate() As Integer, ByVal aEDate() As Integer, ByRef aRetcod As Integer)
    End Sub

    Dim gOutFileStream As IO.FileStream

    Sub Main()

        'local(variables)
        Dim WDMSFL, RETCOD, DSN, DSTYPE, I As Integer
        Dim WDNAME, WDMNAMES(3) As String
        Dim lOutStream() As Byte

        Debug.Print("Start WdmEntDriverFortran")

        If (Find_Test_Folder()) Then
            WDMNAMES(1) = "test.wdm"
            WDMNAMES(2) = "missing.wdm"
            WDMNAMES(3) = "corrupt.wdm"

            For I = 1 To 3
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
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBOPN Return Code " & RETCOD & " Opening " & Trim(WDNAME) & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                Else
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDBOPN Return Code " & WDMSFL & " Opening " & Trim(WDNAME) & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                    DSN = 39
                    DSTYPE = F90_WDCKDT(WDMSFL, DSN)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDCKDT: DSN, TYPE: " & DSN & ", " & DSTYPE & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

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

                    Dim lDates(6) As Integer
                    Dim lDelt As Integer = 1
                    Dim lNvals As Integer = 10
                    Dim lTran As Integer = 0
                    Dim lQual As Integer = 31
                    Dim lTunit As Integer = 4
                    lDates(0) = 1976 : lDates(1) = 4 : lDates(2) = 5 : lDates(3) = 24 : lDates(4) = 0 : lDates(5) = 0 : lDates(6) = 0
                    Dim lRval(10) As Single
                    F90_WDTGET(WDMSFL, DSN, lDelt, lDates, lNvals, lTran, lQual, lTunit, lRval, RETCOD)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDTGET: DSN, RVAL: " & DSN & ", " & lRval(0).ToString & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)

                    RETCOD = F90_WDFLCL(WDMSFL)
                    lOutStream = (System.Text.Encoding.Unicode.GetBytes("  F90_WDFLCL Return Code " & RETCOD & vbCrLf))
                    gOutFileStream.Write(lOutStream, 0, lOutStream.Length)
                End If
                gOutFileStream.Flush()
            Next I
        Else
            Debug.Print("Test Folder Not Found")
        End If

        gOutFileStream.Close()
        Debug.Print("End WdmEntDriverFortran")

    End Sub

    Function Find_Test_Folder() As Boolean
        Try
            Dim Cdir As String = CurDir()
            Debug.Print("Base directory is: " & Cdir)
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
