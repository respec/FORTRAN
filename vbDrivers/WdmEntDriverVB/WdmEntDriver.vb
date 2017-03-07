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

    Sub Main()

        'local(variables)
        Dim WDMSFL, RETCOD, DSN, DSTYPE, I As Integer
        Dim WDNAME, WDMNAMES(3) As String

        Debug.Print("Start WdmEntDriverFortran")

        WDMNAMES(1) = "test.wdm"
        WDMNAMES(2) = "missing.wdm"
        WDMNAMES(3) = "corrupt.wdm"

        If (Find_Test_Folder()) Then
            For I = 1 To 3
                WDNAME = WDMNAMES(I)
                WDMSFL = 40
                RETCOD = F90_WDMOPN(WDMSFL, WDNAME, Len(WDNAME))
                Debug.Print("F90_WDMOPN Return Code " & RETCOD & " Opening " & Trim(WDNAME))
                RETCOD = F90_WDMCLO(WDMSFL)
                Debug.Print("F90_WDMCLO Return Code " & RETCOD & " Closing " & WDMSFL)

                WDMSFL = F90_WDBOPN(0, WDNAME, Len(WDNAME))
                If (WDMSFL < 0) Then
                    Debug.Print("F90_WDBOPN Return Code " & RETCOD & " Opening " & Trim(WDNAME))
                Else
                    Debug.Print("F90_WDBOPN Return Code " & WDMSFL & " Opening " & Trim(WDNAME))
                    DSN = 39
                    DSTYPE = F90_WDCKDT(WDMSFL, DSN)
                    Debug.Print("F90_WDCKDT: DSN, TYPE: " & DSN & ", " & DSTYPE)
                End If
                RETCOD = F90_WDFLCL(WDMSFL)
                Debug.Print("F90_WDFLCL Return Code " & RETCOD)
            Next I
        Else
            Debug.Print("Test Folder Not Found")
        End If

        Debug.Print("End WdmEntDriverFortran")

    End Sub

    Function Find_Test_Folder() As Boolean
        Try
            Dim Cdir As String = CurDir()
            Debug.Print("Base directory is: " & Cdir)
            Cdir &= "\..\..\..\test"
            ChDir(Cdir)
            Debug.Print("Test directory is: " & CurDir())
            Find_Test_Folder = True
        Catch
            Debug.Print("Failed to Set Test directory ")
            Find_Test_Folder = False
        End Try
    End Function

End Module
