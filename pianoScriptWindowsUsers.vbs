Set WshShell = CreateObject("WScript.Shell")
strString = "pfulffjpayodhfu18etulfDfup9eulukfayiWtIOTi9wetuoEyiwyosffuosfDftjtu48wypyetyIsetiwtIpdgdaseyiwEYos0wrypauOaOtuwriwtodgdghoauetyiopdawtIOpupadsuosuosfuoetospadIsdkfDfdgriwrtyogdfupasuetuwryiwtiwryuwtu9qryryIssfDfukhjhrie918ey50wtiadyIsfdgfDfDfospdadIpasfkhfodfDfuetukfodgtuodspdgodaptosuoaulkfu18eTiWyoswrypey17oEyiwEYo6etuosauoaOpptyietietosuoEyIsadq9weteyo6etIpyoaspasptuoSulkfdy5918wtulupuswrietu918wtuwtuo6eywtieypetiwey50wti8pukjhfDfDfulukffstudhjkfdgryiweyiwti59wrieyIpuo6eyospdyiWtetusfkfhfu5qwtiWtowry50wrypaOtieu59payiastupawryietiwtusfDfuotietiWyiauodqety59wtyjkhftuwetyias9qryospyi80wetufulkfffxDfDffDfDfds9qeyoSusfDfu8wrtIpfueyulfuoaukfuodjtukffdsriWtulkfti50wtIpauopdasfosyiwri59wriWyi50wrtu180wTityietjdaddsawtieuosfkfteyos0wtulfustetIsfsadryieyIOpawri5qeyituosupdayietiwtyi9wetuoSulfseypayi50weti6etietioadsaqetjdgdri9wtuosfdsuoeyiWtuoSukfdhgriEy59wrtulfudsuwti9wtogtieyIpadhjtufukfffukfosuoetuo8wy5qeti59wtjhfsfkflkjdhfjpaOOayodjhjs0wyoypaukfjkfkhgDfdftiWtiwrietuosusfos0qeyIpdadsulful"

WshShell.AppActivate "Chrome"
WScript.sleep 5000
For i=1 To Len(strString)
    WshShell.SendKeys Mid(strString,i,1)
	WScript.sleep 350
Next
