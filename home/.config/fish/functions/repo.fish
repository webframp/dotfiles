# Defined in /var/folders/66/180ynjt55z74l338gh0stq0c0000gn/T//fish.0eDUoI/repo.fish @ line 2
function repo
	set --local sshremote (git remote -v | grep -e 'git@' -e 'git:' -c);
  set --local httpremote (git remote -v | grep 'http' -c);
    if test $sshremote -ne 0
        set repo_url (git remote -v|grep -Eo 'git\@[^ ]+' -m1| sed -e 's/:/\//' | sed -e 's/git\@/https:\/\//'| sed -e 's/\.git//');
    else if test $httpremote -ne 0
        set repo_url (git remote -v|grep -Eo 'http\.*[^ ]+' -m1);
    else
        set repo_url "none"
    end
    if test $repo_url != "none"
        echo "Opening $repo_url";
        command open $repo_url;
    else
        echo "No repo url found";
    end
end
