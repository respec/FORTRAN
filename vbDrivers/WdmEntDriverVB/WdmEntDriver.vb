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
