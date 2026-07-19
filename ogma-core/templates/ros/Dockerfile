FROM osrf/space-ros:humble-2024.10.0

ARG USER=spaceros-user
ARG PACKAGE_PATH=/home/${USER}/monitors
ARG ROS_PATH=/home/${USER}/spaceros/

RUN mkdir -p ${PACKAGE_PATH}/src/
ADD copilot ${PACKAGE_PATH}/src/copilot
ADD test_requirements ${PACKAGE_PATH}/src/test_requirements
USER root
RUN chown -R ${USER} ${PACKAGE_PATH}
USER ${USER}

SHELL ["/bin/bash", "-c"]
WORKDIR ${PACKAGE_PATH}

ADD --chmod=644 https://raw.githubusercontent.com/ros/rosdistro/master/ros.key /usr/share/keyrings/ros-archive-keyring.gpg
RUN sudo apt-get update

ENV RMW_IMPLEMENTATION=rmw_cyclonedds_cpp
ENV ROS_DISTRO=humble

ADD manual-deps*.repos /tmp/
RUN if [ -f "/tmp/manual-deps.repos" ]; then \
      vcs import src < /tmp/manual-deps.repos; \
    fi

ADD excluded-pkgs*.txt /tmp/
RUN rosdep update
RUN source /opt/spaceros/install/setup.bash && \
    if [ -f "/tmp/excluded-pkgs.txt" ]; then \
      rosdep install -y \
        --from-paths src --ignore-src \
        --rosdistro ${ROS_DISTRO} \
        --skip-keys "$(tr '\n' ' ' < '/tmp/excluded-pkgs.txt')"; \
    else \
      rosdep install -y \
        --from-paths src --ignore-src \
        --rosdistro ${ROS_DISTRO} ; \
    fi

ADD manually-installed-pkgs*.txt /tmp/
RUN source /opt/spaceros/install/setup.bash && \
    colcon build --packages-select \
      copilot \
      test_requirements \
      $(find /tmp/ -maxdepth 1 -name "manually-installed-pkgs.txt" -exec cat {} +)

ADD screenrc /home/spaceros-user/.screenrc
USER root
ADD screenrc /root/.screenrc
RUN chown -R spaceros-user /home/spaceros-user/.screenrc
USER spaceros-user
