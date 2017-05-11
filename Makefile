build:
	stack build

deploy:
	./scripts/deploy.sh

undeploy:
	rm -R .deploy-work
