source .env

git config user.name "$USER_NAME"

git add .
git commit -m "$1"

git push origin main

echo "DONE"
