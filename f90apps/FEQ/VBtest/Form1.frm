VERSION 4.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   2352
   ClientLeft      =   1248
   ClientTop       =   1236
   ClientWidth     =   3264
   Height          =   2676
   Left            =   1200
   LinkTopic       =   "Form1"
   ScaleHeight     =   2352
   ScaleWidth      =   3264
   Top             =   960
   Width           =   3360
   Begin VB.TextBox Text1 
      Height          =   288
      Index           =   3
      Left            =   1800
      TabIndex        =   5
      Text            =   "0"
      Top             =   360
      Width           =   612
   End
   Begin VB.TextBox Text1 
      Height          =   288
      Index           =   2
      Left            =   360
      TabIndex        =   4
      Text            =   "1"
      Top             =   1080
      Width           =   612
   End
   Begin VB.TextBox Text1 
      Height          =   288
      Index           =   1
      Left            =   360
      TabIndex        =   3
      Text            =   "1"
      Top             =   720
      Width           =   612
   End
   Begin VB.TextBox Text1 
      Height          =   288
      Index           =   0
      Left            =   360
      TabIndex        =   2
      Text            =   "1996"
      Top             =   360
      Width           =   612
   End
   Begin VB.CommandButton Command1 
      Caption         =   "J2G Day"
      Height          =   492
      Index           =   1
      Left            =   1800
      TabIndex        =   1
      Top             =   1560
      Width           =   1092
   End
   Begin VB.CommandButton Command1 
      Caption         =   "G2J Day"
      Height          =   492
      Index           =   0
      Left            =   360
      TabIndex        =   0
      Top             =   1560
      Width           =   1092
   End
End
Attribute VB_Name = "Form1"
Attribute VB_Creatable = False
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click(Index As Integer)
    Dim j&, y&, m&, d&
    If Index = 0 Then
      If IsNumeric(Text1(0)) And IsNumeric(Text1(1)) And IsNumeric(Text1(2)) Then
        y = CLng(Text1(0))
        m = CLng(Text1(1))
        d = CLng(Text1(2))
        j = F90_MJD(y, m, d)
        Text1(3) = CStr(j)
      Else
        MsgBox "Bad YMD"
      End If
    Else
      If IsNumeric(Text1(3)) Then
        j = CLng(Text1(3))
        Call F90_INVMJD(j, y, m, d)
        Text1(0) = CStr(y)
        Text1(1) = CStr(m)
        Text1(2) = CStr(d)
      Else
        MsgBox "Bad JDay"
      End If
    End If
End Sub


