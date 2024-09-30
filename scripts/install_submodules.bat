cd ..

mkdir 3rd

cd 3rd

git clone --depth=1 https://github.com/skalogryz/richmemo.git
git clone https://github.com/blikblum/multilog.git
cd multilog
git reset --hard a271010
cd ..
git clone --depth=1 http://git.code.sf.net/p/lazarus-ccr/dcpcrypt
git clone https://github.com/Slasar41/internettools.git
git clone https://github.com/blikblum/VirtualTreeView-Lazarus.git
cd VirtualTreeView-Lazarus
git checkout lazarus-v5
cd ..
svn checkout https://svn.code.sf.net/p/synalist/code/trunk synalist-code

lazbuild --add-package-link synalist-code/laz_synapse.lpk
lazbuild --add-package-link dcpcrypt/dcpcrypt.lpk
lazbuild --add-package-link internettools/internettools.lpk
lazbuild --add-package-link richmemo/richmemopackage.lpk
lazbuild --build-ide= --add-package VirtualTreeView-Lazarus/Source/virtualtreeview_package.lpk
lazbuild --build-ide= --add-package richmemo/ide/richmemo_design.lpk
lazbuild --build-ide= --add-package multilog/multiloglaz.lpk

cd ..