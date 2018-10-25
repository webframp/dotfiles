# Defined in /var/folders/66/180ynjt55z74l338gh0stq0c0000gn/T//fish.vEMNsi/repo.fish @ line 2
function repo
    set --local isitgit (git remote -v | grep 'git@' -c);
    if test $isitgit -ne 0
        set --local repo (git remote -v|grep -Eo 'git\@[^ ]+' -m1| sed -e 's/:/\//' | sed -e 's/git\@/https:\/\//'| sed -e 's/\.git//');
        echo "Opening $repo";
        command open $repo;
    end
end
