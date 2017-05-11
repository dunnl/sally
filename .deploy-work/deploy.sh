ROOT=$(stack path --project-root)
WORKDIR_REL=.deploy-work
WORKDIR_ABS="$ROOT"/"$WORKDIR_REL"
cd $ROOT
echo "Using $WORKDIR_ABS as working directory for deployment files"
mkdir $WORKDIR_REL
stack install --local-bin-path $WORKDIR_ABS
cp -R scripts/ $WORKDIR_ABS
tar -zcf sally.tar.gz -C $WORKDIR_REL .
