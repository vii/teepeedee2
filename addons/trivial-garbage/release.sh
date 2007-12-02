#!/bin/sh

VERSION=${VERSION:=`grep :version trivial-garbage.asd | cut -d\" -f2`}
TARBALL_NAME="trivial-garbage_$VERSION"
TARBALL="$TARBALL_NAME.tar.gz"
SIGNATURE="$TARBALL.asc"
RELEASE_DIR=${RELEASE_DIR:="public_html/tarballs/"}

echo "Tagging the tree..."
darcs tag "$VERSION"

echo "Creating distribution..."
darcs dist -d "$TARBALL_NAME"

echo "Signing tarball..."
gpg -b -a "$TARBALL_NAME.tar.gz"

echo "Copying tarball to web server..."
scp "$TARBALL" "$SIGNATURE" common-lisp.net:"$RELEASE_DIR"

echo "Uploaded $TARBALL and $SIGNATURE."
echo "Don't forget to update the link on the CLiki page!"
