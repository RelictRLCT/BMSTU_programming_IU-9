#include <iostream>
#include "GL/freeglut.h"
#include "GL/gl.h"
#include "GLFW/glfw3.h"
using namespace std;
int angle = 0;
float deltaposx = 0;
float deltaposy = 0;
float posx1 = -0;
float posx2 = -0.5;
float posx3 = 0.5;
float posx4 = 0.5;
float posy1 = -0.5;
float posy2 = 0.2;
float posy3 = 0.3;
float posy4 = -0.5;


void display(GLFWwindow *window) {

    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
    glClearColor(0.5, 0.7, 0.5, 0.0);

    glPushMatrix();

    glRotatef(angle, 0, 0, 1);

    glBegin(GL_POLYGON);
    glColor3f(0.3, 0.1, 0.8);
    glVertex2f(posx1, posy1);
    glColor3f(1.0, 0.6, 0.8);
    glVertex2f(posx2, posy2);
    glColor3f(1.0, 0.1, 0.1);
    glVertex2f(posx3, posy3);
    glColor3f(1.0, 0.7, 0.5);
    glVertex2f(posx4, posy4);
    glEnd();

    glPopMatrix();

    glfwSwapBuffers(window);
    glfwPollEvents();
}

void CallbackKeys(GLFWwindow* window, int key, int scancode, int action, int mods) {

    if (action == GLFW_PRESS) {
        switch (key) {
            case GLFW_KEY_RIGHT:
                deltaposx = 0.1;
                posx1 += deltaposx;
                posx2 += deltaposx;
                posx3 += deltaposx;
                posx4 += deltaposx;
                break;

            case GLFW_KEY_LEFT:
                deltaposx = -0.1;
                posx1 += deltaposx;
                posx2 += deltaposx;
                posx3 += deltaposx;
                posx4 += deltaposx;
                break;

            case GLFW_KEY_UP:
                deltaposy = 0.1;
                posy1 += deltaposy;
                posy2 += deltaposy;
                posy3 += deltaposy;
                posy4 += deltaposy;
                break;

            case GLFW_KEY_DOWN:
                deltaposy = -0.1;
                posy1 += deltaposy;
                posy2 += deltaposy;
                posy3 += deltaposy;
                posy4 += deltaposy;
                break;
        }
    }
}

void CallbackScroll(GLFWwindow* window, double xoffset, double yoffset) {

    if (yoffset > 0) {
        angle += 3;
    } else {
        angle -=3;
    }
}

int main() {

    glfwInit();
    GLFWwindow *window;

    window = glfwCreateWindow(840, 840, "Laba 1", NULL, NULL);
    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, CallbackKeys);
    glfwSetScrollCallback(window, CallbackScroll);
    while (not glfwWindowShouldClose(window)) {
        display(window);
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
