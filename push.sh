source .env

git config --global user.name "$USER_NAME"
git config --global user.password "$GIT_KEY"

git add .
git commit -m "$1"

git push origin main

echo "DONE"

