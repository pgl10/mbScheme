                   
In order to use this code please add your data in the data.scm file then you may run the matrix.bat file.
                   
Look at the examples.txt file to see some commands and their answers.
                   
If you want to keep some results you may write commands as below.
                   
>> (define myfile (open "archive.txt"))
>> (fdisplay myfile ">> M4")
>> (new-line myfile)
>> (fdisplay myfile (matrix-get-row M4 0))
>> (new-line myfile)
>> (fdisplay myfile (matrix-get-row M4 1))
>> (new-line myfile)
>> (fdisplay myfile (matrix-get-row M4 2))
>> (new-line myfile)
>> (fdisplay myfile (matrix-get-row M4 3))
>> (new-line myfile)
>> (define d (matrix-det M4))
>> (fdisplay myfile ">> d")
>> (new-line myfile)
>> (fdisplay myfile d)
>> (new-line myfile)
>> (define b (matrix-mul-vec M4 (vector 1 2 3 4)))
>> (fdisplay myfile ">> b")
>> (new-line myfile)
>> (fdisplay myfile b)
>> (new-line myfile)
>> (define x (matrix-sys-eqt M4 b))
>> (fdisplay myfile ">> x")
>> (new-line myfile)
>> (fdisplay myfile x)
>> (new-line myfile)
>> (file-close! myfile)
                   
With this example you should get this in the archive.txt file.
                   
>> M4
[4 8 7 9]
[6 8 2 4]
[5 7 6 3]
[1 3 7 9]
>> d
-310
>> b
[77 44 49 64]
>> x
[1 2 3 4]
                   
