structureFolder <- function(path, ...) {
    img.folder <- paste(path, 'IMG/', sep='/')
    ref.folder <- paste(path, 'REF/', sep='/')
    roi.folder <- paste(path, 'ROI/', sep='/')
    VI.folder <- paste(path, 'VI/', sep='/')
    dir.create(img.folder,...)
    dir.create(ref.folder,...)
    dir.create(VI.folder,...)
    dir.create(roi.folder,...)    
cat(paste('Put all your images in', img.folder, '\n', sep=' '))
cat(paste('Put your reference image in', ref.folder, '\n', sep=' '))    
cat(paste('Draw your ROI with DrawROI():\n set path_img_ref to ', ref.folder, '\n','set path_ROIs to', roi.folder,'\n', sep=' '))
cat(paste('Then you can extractVIs():', '\n', 'set img.path as', img.folder,
          '\n', 'set roi.path as', roi.folder, '\n',
          'set vi.path to',VI.folder,'\n',sep=' '))
cat('------------------------\n')    
cat('Alternatively, assign this function to an object and use named elements of the returned list\n')    
invisible(list(img=img.folder, ref=ref.folder, roi=roi.folder, VI=VI.folder))
}
