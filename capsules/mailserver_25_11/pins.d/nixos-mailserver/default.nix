{ lib, legacyPackages, ... }:
let
    esc = lib.strings.escapeShellArg;
    gitlab_repo = "simple-nixos-mailserver/nixos-mailserver";
    git_branch = "nixos-25.11";
in {
    inherit gitlab_repo git_branch;
    lock = import ./lock.nix;
    updateScript = let
        nix-prefetch-url = "${legacyPackages.nix}/bin/nix-prefetch-url";
        git = "${legacyPackages.git}/bin/git";
    in ''
        gitlab_repo=${esc gitlab_repo}
        gitlab_project=''${gitlab_repo##*/}
        git_repo=https://gitlab.com/"$gitlab_repo"
        git_branch=${esc git_branch}
        
        determine_head() {
        	local checkout_dir
        	checkout_dir=$(mktemp -d)
        	(
        		cd -- "$checkout_dir"
        		${git} clone --depth 1 --single-branch --branch "$git_branch" --quiet -- "$git_repo" .
        		${git} rev-parse HEAD
        	)
        	rm -rf -- "$checkout_dir"
        }
        
        git_commit=$(determine_head)
        
        current_url="$git_repo"/-/archive/"$git_commit"/"$gitlab_project"-"$git_commit".tar.gz
        current_sha=$(${nix-prefetch-url} --unpack "$current_url")
        cat > ${esc (toString ./lock.nix)} <<-EOF
        	# GENERATED
        	with builtins;
        	rec {
        	    url = "$current_url";
        	    sha256 = "$current_sha";
        	    nix_path = fetchTarball {
        	        inherit url sha256;
        	    };
        	}
        EOF
    '';
}
