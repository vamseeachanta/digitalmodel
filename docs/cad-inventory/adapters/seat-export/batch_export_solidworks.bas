' ============================================================================
'  Batch STEP export for SolidWorks  (state-of-readiness macro)
'  Reads a text file of full part/assembly paths (one per line) and exports
'  each to STEP, mirroring the source tree under an output root.
'
'  HOW TO RUN (on a Windows machine with a licensed SolidWorks seat):
'    1. Tools > Macro > New ... paste this in, or Tools > Macro > Edit this .bas
'    2. Edit the three CONFIG constants below (LIST_FILE, OUT_ROOT, SRC_ROOT).
'    3. Tools > Macro > Run.  TEST on a 5-line list first, verify the .step files
'       open in a viewer, THEN run the full batch.
'
'  Output: <OUT_ROOT>\<relative path>\<name>.step   + a log file next to LIST_FILE.
'  Notes:
'    - Assemblies (.sldasm) require their referenced parts to resolve; run from a
'      tree whose references are intact (the active/curated project copies).
'    - AP214 is the default (broadest compatibility). Set STEP_AP = 242 for
'      AP242 (PMI) if your SolidWorks version supports it.
' ============================================================================
Dim swApp As Object

Const LIST_FILE As String = "C:\step-export\batch_list.txt"   ' <-- one source path per line
Const OUT_ROOT  As String = "C:\step-export\out"              ' <-- mirror output here
Const SRC_ROOT  As String = "Z:\"                             ' <-- drive mapping to the share root (e.g. \\ace-linux-1\ace)
Const STEP_AP   As Long   = 214                               ' 214 or 242

Sub main()
    Set swApp = Application.SldWorks
    swApp.SetUserPreferenceIntegerValue swUserPreferenceIntegerValue_e.swStepAP, STEP_AP
    swApp.SetUserPreferenceToggle swUserPreferenceToggle_e.swStepExportOption3DCurves, True

    Dim fso As Object: Set fso = CreateObject("Scripting.FileSystemObject")
    Dim ts As Object: Set ts = fso.OpenTextFile(LIST_FILE, 1)   ' ForReading
    Dim logf As Object: Set logf = fso.CreateTextFile(LIST_FILE & ".log", True)
    Dim nOk As Long, nErr As Long, line As String

    Do Until ts.AtEndOfStream
        line = Trim(ts.ReadLine)
        If Len(line) > 0 Then
            On Error Resume Next
            Dim ext As String: ext = LCase(Right(line, 7))
            Dim docType As Long
            If InStr(ext, ".sldasm") > 0 Then docType = swDocASSEMBLY Else docType = swDocPART

            Dim errs As Long, warns As Long
            Dim doc As Object
            Set doc = swApp.OpenDoc6(line, docType, swOpenDocOptions_Silent, "", errs, warns)

            If Not doc Is Nothing Then
                ' build mirrored output path
                Dim rel As String: rel = line
                If InStr(1, LCase(line), LCase(SRC_ROOT)) = 1 Then rel = Mid(line, Len(SRC_ROOT) + 1)
                Dim outPath As String: outPath = OUT_ROOT & "\" & rel
                outPath = Left(outPath, InStrRev(outPath, ".") - 1) & ".step"
                EnsureDir fso, fso.GetParentFolderName(outPath)

                Dim sErr As Long, sWarn As Long
                doc.Extension.SaveAs outPath, 0, 1, Nothing, sErr, sWarn   ' silent SaveAs (.step from extension)
                If sErr = 0 Then nOk = nOk + 1 Else nErr = nErr + 1
                logf.WriteLine IIf(sErr = 0, "OK   ", "FAIL ") & line & " -> " & outPath
                swApp.CloseDoc doc.GetTitle
            Else
                nErr = nErr + 1
                logf.WriteLine "OPENFAIL " & line
            End If
            On Error GoTo 0
        End If
    Loop
    ts.Close
    logf.WriteLine "DONE  ok=" & nOk & "  err=" & nErr
    logf.Close
    swApp.SendMsgToUser "Batch STEP export done. OK=" & nOk & "  ERR=" & nErr
End Sub

Sub EnsureDir(fso As Object, path As String)
    If Len(path) = 0 Then Exit Sub
    If Not fso.FolderExists(path) Then
        EnsureDir fso, fso.GetParentFolderName(path)
        fso.CreateFolder path
    End If
End Sub
