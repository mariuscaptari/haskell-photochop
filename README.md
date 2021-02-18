# Haskell Photochop
CLI image editor for PPM (*Portable Pixmap Format*) images.

### Setup
Install the libraries necessary
```
cabal install --lib QuickCheck
cabal install --lib random
```

### Compile
```
ghc --make main.hs -o photochop  
```

### Run
The **first** argument is the base image that will stay unchanged. The **second** argument is the name of the second image that will reflect the operations performed on the first.

To simply make a copy of an image:
```
./photochop lena.ppm lena2.ppm 
```

#### List of supported commands
* Flip horizontal **-fh** 
* Flip vertical **-fv**
* Half height **-hh**
* Half width **-hw**
* Grey scale **-gs**
* Red color **-rc**
* Green color **-gc**
* Blue color **-bc**
* Run tests **-t**

To flip an image horizontally and then apply a gray scale:
```
./photochop lena.ppm lena2.ppm -fh -gs
```

### Tested properties
To test the programs output I've created three difrent properties that can be tested using the QuickCheck module. Those properties are:
* Flipping an image twice gives the original image back.
* None of the pixel values are greater than the maximum value shown in the header.
* The number of total pixels should be equal to the resolution of the image.

These properties can be tested with the following command:
```
./photochop -t
```