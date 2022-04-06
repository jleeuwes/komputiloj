#!/usr/bin/env bash

## WARNING
## I was editing this but than I nuked all changes.
## The changes are in changes-i-accidentally-nuked.patch
## but I wasn't able to apply that patch.
## So do that first when you go work on this!
## (Or start again, whatever.)

set -Eeu -o pipefail

try_mail() {
	local from="$1"
	local to="$2"
	printf "Trying mail from %q to %q through %q...\n" "$from" "$to" "$MAIL_SERVER" >&2
	# We don't send QUIT so we don't get a Bye so we can just
	# regard the last line as the result.
	# This does mean the server won't end the connection,
	# so we give a timeout to nc to do it ourselves.
	(ssh root@gently.radstand.nl bash <<-EOF
		nc -w 1 $MAIL_SERVER 25 <<-FOE
			EHLO test
			MAIL FROM: <$from>
			RCPT TO: <$to>
		FOE
	EOF
	) | tail -n 1
}

match() {
	# Warning: value will most likely contain a trailing newline,
	# so best use a pattern that ends with a *
	local pattern="$1"
	local value="$2"
	if [[ "$value" == $pattern ]]; then
		printf ":) Got %q\n" "$value" >&2
	else
		printf ":( Expected %q\n:( Got %q\n" "$pattern" "$value" >&2
		return 1
	fi
}

# Note: these tests assume that lwstn.eu is not handled by our servers
# (which is currently the case and probably will remain the case),
# so we can use that as an external address without too much risk
# if we accidentally send an actual mail.

test_external() {
	# Emulate an external SMTP server
	# by going through the public IP of our mailserver.
	# We need to do this from some server of ours,
	# because home ISPs block port 25.
	# To make the test more robust with respect to where it will run,
	# we hardcode the IP address.
	MAIL_SERVER=164.138.27.107

	local result

	# Mail originating from outside destined for radstand.nl should be fine
	# (ignoring DKIM etc for a moment)
	match "250 2.1.5 *" "$(try_mail jeroen@lwstn.eu jeroen@radstand.nl)"
	match "250 2.1.5 *" "$(try_mail jeroen@lwstn.eu jeroen@gorinchemindialoog.nl)"
	match "550 5.1.1 *" "$(try_mail jeroen@lwstn.eu pietjepuk@radstand.nl)" # doesn't exist
	
	# Ideally we would not allow external mail that pretends to come from
	# one of our domains. This should be implemented using DKIM and/of SPF.
	# match "xxx" "$(try_mail jeroen@radstand.nl pietjepuk@radstand.nl)" # doesn't exit
	# match "xxx" "$(try_mail pukjepiet@radstand.nl pietjepuk@radstand.nl)"
	# match "xxx" "$(try_mail pietjepuk@radstand.nl jeroen@radstand.nl)"
	# match "xxx" "$(try_mail jeroen@radstand.nl jeroen@lwstn.eu)"
	
	# Outgoing mail should NOT be allowed exernally
	match "454 4.7.1 *" "$(try_mail jeroen@radstand.nl jeroen@lwstn.eu)"
	match "454 4.7.1 *" "$(try_mail jeroen@radstand.nl jeroenleeuwestein@a-eskwadraat.nl)"

	# Mail that neither comes from or is destined for our server should be
	# a big no-no!
	match "454 4.7.1 *" "$(try_mail jeroen@lwstn.eu jeroenleeuwestein@a-eskwadraat.nl)"
	match "454 4.7.1 *" "$(try_mail jeroenleeuwestein@a-eskwadraat.nl jeroen@lwstn.eu)"
}

test_internal() {
	# Emulate an relay from inside our network.
	MAIL_SERVER=10.0.0.2

	local result

	# Mail originating from outside destined for radstand.nl should be fine
	# (ignoring DKIM etc for a moment)
	match "250 2.1.5 *" "$(try_mail jeroen@lwstn.eu jeroen@radstand.nl)"
	match "250 2.1.5 *" "$(try_mail jeroen@lwstn.eu jeroen@gorinchemindialoog.nl)"
	match "550 5.1.1 *" "$(try_mail jeroen@lwstn.eu pietjepuk@radstand.nl)" # doesn't exist
	
	# Check some internal-to-internal mails.
	match "250 2.1.5 *" "$(try_mail pietjepuk@radstand.nl jeroen@radstand.nl)"
	match "250 2.1.5 *" "$(try_mail jeroen@radstand.nl jeroen@gorinchemindialoog.nl)"

	# Outgoing mail should be allowed internally
	match "250 2.1.5 *" "$(try_mail jeroen@radstand.nl jeroen@lwstn.eu)"
	match "250 2.1.5 *" "$(try_mail jeroen@radstand.nl jeroenleeuwestein@a-eskwadraat.nl)"
	# TODO what about mail from *@gorinchemindialoog.nl?
	
	# Mail that neither comes from or is destined for our server should be
	# a big no-no!
	# Internally, we do allow anything,
	# because local relay is used for outgoing mail.
	# This is not ideal, but it was the easily configurable route.
	# It's okay as long as we fully trust all users on our system to not send mails with fake from addresses.
	# Better would be to disallow arbitrary relay locally just like externally,
	# and require authenticated mail submission with some sort of From check.
	match "250 2.1.5 *" "$(try_mail jeroen@lwstn.eu jeroenleeuwestein@a-eskwadraat.nl)"
	match "250 2.1.5 *" "$(try_mail jeroenleeuwestein@a-eskwadraat.nl jeroen@lwstn.eu)"
}

test_external
test_internal
