# 3Dbullets

This package contains various methods for the preprocessing of 3D bullet cartridge image files in order to make comparisons between cartridge images.

The only two methods that aren't helper functions, are "rotate" and "bilrot". All other methods are simply used by these two main methods.

The "rotate" method levels out the image. Specifically, if there was an overall slant in the 3D image, this method returns a new image whose best fit plane is a level plane.

The "bilrot" method rotates an image by a specified theta, using bilinear interpolation to fill in the values at noninteger coordinates that result from multiplying by the rotation matrix.
