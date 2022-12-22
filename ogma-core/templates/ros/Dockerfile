FROM osrf/space-ros:latest

ARG USER=spaceros-user
ARG PACKAGE_PATH=/home/${USER}/monitors
ARG ROS_PATH=/home/${USER}/spaceros/

RUN mkdir -p ${PACKAGE_PATH}/src/
ADD copilot ${PACKAGE_PATH}/src/copilot
ADD test_requirements ${PACKAGE_PATH}/demo/test_requirements
USER root
RUN chown -R ${USER} ${PACKAGE_PATH}
USER ${USER}

SHELL ["/bin/bash", "-c"]
WORKDIR ${PACKAGE_PATH}
RUN source ${ROS_PATH}/install/setup.bash && \
    colcon build --packages-select copilot           --paths copilot/          --install-base ${ROS_PATH}/install/ && \
    colcon build --packages-select test_requirements --paths test_requirements --install-base ${ROS_PATH}/install/

ADD screenrc /home/spaceros-user/.screenrc
USER root
RUN chown -R spaceros-user /home/spaceros-user/.screenrc
USER spaceros-user
