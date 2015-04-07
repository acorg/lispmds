(in-package user)


;;;----------------------------------------------------------------------
;;;                 Use Matlab for 3D visualization
;;;----------------------------------------------------------------------

(defun 0-to-255-into-0-1 (x)
  (/ x 255.0))

(defun 3d-save-to-matlab-file (save filename &optional &key (include-names t))
  (let* ((coordss (mapcar (^ (l) (mapcar (^ (x) (dps x 6)) l)) (coordss (starting-coordss-from-save save))))
	 (colors (mapcar (^ (x) (mapcar #'0-to-255-into-0-1 (if (equal "{}" x) '(0 0 0) (tk-color-to-rgb x)))) (coords-colors-from-save save)))
	 (names (hi-table-antigens (table-from-save save))))
    (fll (mapcar #'apply-append (if include-names
				    (transpose coordss colors (mapcar #'list names))
				  (transpose coordss colors)))
	 :filename filename)))



#|
;; ----------------- the antigenic map ---------------------------

(length
 (3d-save-to-matlab-file
  (fi-in "/home/dsmith/mds/investigations/merge-hi-tables/seq-t9a-mod27-3d.save")
  "/home/dsmith/mds/investigations/merge-hi-tables/seq-t9a-mod27-3d-for-matlab"))

;; and 2d data (for ron), matlab code would need to be changed to read in
(length
 (3d-save-to-matlab-file
  (fi-in "/home/dsmith/mds/investigations/merge-hi-tables/seq-t9a-mod27.save")
  "/home/dsmith/mds/investigations/merge-hi-tables/seq-t9a-mod27-2d-for-matlab"))


;; --------------------- matlab code for the antigenic map -----------------------------
[x,y,z,r,g,b,name]=textread('/home/dsmith/mds/investigations/merge-hi-tables/seq-t9a-mod27-3d-for-matlab', '%f %f %f %f %f %f %s');
[sX,sY,sZ]=sphere(10);
o11=ones(11,11);
%for i=1:size(x,1)
for i=1:273
  c=zeros(11,11,3);
  c(:,:,1)=r(i)*ones(11,11);
  c(:,:,2)=g(i)*ones(11,11);
  c(:,:,3)=b(i)*ones(11,11);
  hold on
  lips=1
  surf(sX*lips+o11*x(i)*5, sY*lips+o11*y(i)*5, sZ+o11*z(i)*5, c, 'EdgeColor', 'none')
end
for i=274:size(x,1)
  c=zeros(11,11,3);
  c(:,:,1)=r(i)*ones(11,11);
  c(:,:,2)=g(i)*ones(11,11);
  c(:,:,3)=b(i)*ones(11,11);
  hold on
  lips=1
  mesh(sX*lips+o11*x(i)*5, sY*lips+o11*y(i)*5, sZ+o11*z(i)*5, c)
end
axis equal
axis off
text(x*5, y*5, z*5, name)


;; ----------------- the genetic map ---------------------------

(length
 (3d-save-to-matlab-file
  (fi-in "/home/dsmith/mds/investigations/sequence-mds/2004-02-mt30-3d-hi-subset-ag-map-colored.save")
  "/home/dsmith/mds/investigations/sequence-mds/2004-02-mt30-3d-hi-subset-ag-map-colored-for-matlab"))

;; --------------------- matlab code for the genetic map -----------------------------
[x,y,z,r,g,b,name]=textread('/home/dsmith/mds/investigations/sequence-mds/2004-02-mt30-3d-hi-subset-ag-map-colored-for-matlab', '%f %f %f %f %f %f %s');
[sX,sY,sZ]=sphere(10);
o11=ones(11,11);
for i=1:size(x,1)
  c=zeros(11,11,3);
  c(:,:,1)=r(i)*ones(11,11);
  c(:,:,2)=g(i)*ones(11,11);
  c(:,:,3)=b(i)*ones(11,11);
  hold on
  lips=1
  surf(sX*lips+o11*x(i)*1, sY*lips+o11*y(i)*1, sZ+o11*z(i)*1, c, 'EdgeColor', 'none')
end
axis equal
axis off
text(x*1, y*1, z*1, name)

|#