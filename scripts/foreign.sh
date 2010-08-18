#!/bin/bash

cd plugins/

svn_repos=( "http://geben-on-emacs.googlecode.com/svn/trunk geben"
    "http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs dsvn"
    )

hg_repos=( "https://emacs-soap-client.googlecode.com/hg/ emacs-soap-client" )


case $1 in
    add )
        echo "adding"

        len=${#svn_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            svn co ${svn_repos[$i]}
        done

        len=${#hg_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            hg clone ${hg_repos[$i]}
        done
        ;;
    update )
        echo "updating" 

        len=${#svn_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            name=`echo ${svn_repos[$i]} | cut -d " " -f 2`
            cd $name
            svn up
            cd ..
        done

        len=${#hg_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            name=`echo ${hg_repos[$i]} | cut -d " " -f 2`
            
            cd $name
            hg pull
            hg update
            cd ..
        done

        ;;
    status )
        echo "status"

        len=${#svn_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            name=`echo ${svn_repos[$i]} | cut -d " " -f 2`
            cd $name
            echo $name
            svn -u status
            cd ..
        done

        len=${#hg_repos[@]}
        for (( i = 0 ; i < len ; i++ ))  
        do
            name=`echo ${hg_repos[$i]} | cut -d " " -f 2`
            
            cd $name
            echo $name
            hg status
            cd ..
        done

        ;;
esac
