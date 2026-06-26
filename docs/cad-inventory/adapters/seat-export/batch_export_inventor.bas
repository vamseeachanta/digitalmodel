' ============================================================================
'  Batch STEP export for Autodesk Inventor  (state-of-readiness macro)
'  Reads a text file of full .ipt/.iam paths (one per line) and exports each to
'  STEP via the Inventor STEP Translator AddIn, mirroring the source tree.
'
'  HOW TO RUN (Windows machine with a licensed Inventor seat):
'    1. Tools > VBA Editor.  Insert > Module.  Paste this in.
'    2. Edit the CONFIG constants (LIST_FILE, OUT_ROOT, SRC_ROOT, STEP_AP).
'    3. Run main.  TEST on a 5-line list first; verify the .stp files, then run all.
'
'  Output: <OUT_ROOT>\<relative path>\<name>.stp  + a .log next to LIST_FILE.
'  Notes:
'    - Assemblies (.iam) resolve referenced .ipt; run from a tree with intact refs.
'    - ApplicationProtocolType: 2=AP203, 3=AP214, 4=AP242 (if supported).
' ============================================================================
Const LIST_FILE As String = "C:\step-export\batch_list.txt"
Const OUT_ROOT  As String = "C:\step-export\out"
Const SRC_ROOT  As String = "Z:\"                  ' share drive mapping
Const STEP_AP   As Long   = 3                      ' 3=AP214, 4=AP242

Public Sub main()
    Dim app As Inventor.Application: Set app = ThisApplication
    Dim fso As Object: Set fso = CreateObject("Scripting.FileSystemObject")
    Dim ts As Object: Set ts = fso.OpenTextFile(LIST_FILE, 1)
    Dim logf As Object: Set logf = fso.CreateTextFile(LIST_FILE & ".log", True)

    ' STEP translator add-in
    Dim stepAddIn As TranslatorAddIn
    Set stepAddIn = app.ApplicationAddIns.ItemById("{90AF7F40-0C01-11D5-8E83-0010B541CD80}")

    Dim nOk As Long, nErr As Long, line As String
    Do Until ts.AtEndOfStream
        line = Trim(ts.ReadLine)
        If Len(line) > 0 Then
            On Error Resume Next
            Dim doc As Inventor.Document
            Set doc = app.Documents.Open(line, False)   ' open invisible
            If Not doc Is Nothing Then
                Dim ctx As TranslationContext: Set ctx = app.TransientObjects.CreateTranslationContext
                ctx.Type = kFileBrowseIOMechanism
                Dim opts As NameValueMap: Set opts = app.TransientObjects.CreateNameValueMap
                Dim med As DataMedium: Set med = app.TransientObjects.CreateDataMedium
                If stepAddIn.HasSaveCopyAsOptions(doc, ctx, opts) Then
                    opts.Value("ApplicationProtocolType") = STEP_AP
                End If

                Dim rel As String: rel = line
                If InStr(1, LCase(line), LCase(SRC_ROOT)) = 1 Then rel = Mid(line, Len(SRC_ROOT) + 1)
                Dim outPath As String: outPath = OUT_ROOT & "\" & rel
                outPath = Left(outPath, InStrRev(outPath, ".") - 1) & ".stp"
                EnsureDir fso, fso.GetParentFolderName(outPath)
                med.FileName = outPath

                stepAddIn.SaveCopyAs doc, ctx, opts, med
                If Err.Number = 0 Then nOk = nOk + 1 Else nErr = nErr + 1
                logf.WriteLine IIf(Err.Number = 0, "OK   ", "FAIL ") & line & " -> " & outPath
                doc.Close (True)
            Else
                nErr = nErr + 1: logf.WriteLine "OPENFAIL " & line
            End If
            Err.Clear
            On Error GoTo 0
        End If
    Loop
    ts.Close
    logf.WriteLine "DONE ok=" & nOk & " err=" & nErr
    logf.Close
    MsgBox "Inventor batch STEP export done. OK=" & nOk & " ERR=" & nErr
End Sub

Sub EnsureDir(fso As Object, path As String)
    If Len(path) = 0 Then Exit Sub
    If Not fso.FolderExists(path) Then
        EnsureDir fso, fso.GetParentFolderName(path)
        fso.CreateFolder path
    End If
End Sub
