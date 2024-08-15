#include <iostream>
#include "GL/freeglut.h"
#include "GL/gl.h"
#include "GLFW/glfw3.h"
#include "cmath"
using namespace std;
int anglez = 0;
int anglex = 0;
int angley = 0;
bool PolyFace = true;

void ViewHyperboloid(){
    GLfloat x, y, z, t, u;
    GLfloat radius = 0.5f;
    int gradation = 100;
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
    glLineWidth(3);
    for (u = -1.0; u < 1.0; u += 1.0/gradation)
    {
        glColor3f(120/256.0, (100+u*100)/256.0, 131/256.0);
        glBegin(GL_TRIANGLE_STRIP);
        for (t = 0.0; t < 2.02*M_PI; t += M_PI/gradation)
        {
            x = radius*cosh(u)*cos(t);
            y = 0.5*radius*cosh(u)*sin(t);
            z = radius*sinh(u);
            glVertex3f(x, y, z);
        }
        glEnd();
    }
}

void display(GLFWwindow *window) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    glEnable(GL_DEPTH_TEST);

    glClearColor(0.1, 0.2, 0.4, 0.0);


    GLfloat rotateX[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, static_cast<GLfloat>(cos(anglex * M_PI / 180)), -static_cast<GLfloat>(sin(anglex * M_PI / 180)), 0.0f,
            0.0f, static_cast<GLfloat>(sin(anglex * M_PI / 180)), static_cast<GLfloat>(cos(anglex * M_PI / 180)), 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    GLfloat rotateY[] = {
            static_cast<GLfloat>(cos(angley * M_PI / 180)), 0.0f, static_cast<GLfloat>(sin(angley * M_PI / 180)), 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            -static_cast<GLfloat>(sin(angley * M_PI / 180)), 0.0f, static_cast<GLfloat>(cos(angley * M_PI / 180)), 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    GLfloat rotateZ[] = {
            static_cast<GLfloat>(cos(anglez * M_PI / 180)), -static_cast<GLfloat>(sin(anglez * M_PI / 180)), 0.0f, 0.0f,
            static_cast<GLfloat>(sin(anglez * M_PI / 180)), static_cast<GLfloat>(cos(anglez * M_PI / 180)), 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    glPushMatrix();
    glMatrixMode(GL_MODELVIEW);


    glMultMatrixf(rotateX);
    glMultMatrixf(rotateY);
    glMultMatrixf(rotateZ);

    ViewHyperboloid();

    glPopMatrix();

    glFlush();
    glfwSwapBuffers(window);
    glfwPollEvents();
}


void CallbackKeys(GLFWwindow* window, int key, int scancode, int action, int mods) {

    if (action == GLFW_PRESS) {
        switch (key) {
            case GLFW_KEY_RIGHT:
                angley += 6;
                break;

            case GLFW_KEY_LEFT:
                angley -= 6;
                break;

            case GLFW_KEY_UP:
                anglex -= 6;
                break;

            case GLFW_KEY_DOWN:
                anglex += 6;
                break;

            case GLFW_KEY_LEFT_CONTROL:
                PolyFace = not PolyFace;
                if (PolyFace) {
                    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
                } else {
                    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                }
        }
    }
}

void CallbackScroll(GLFWwindow* window, double xoffset, double yoffset) {

    if (yoffset > 0) {
        anglez += 3;
    } else {
        anglez -=3;
    }
}

int main() {

    glfwInit();
    GLFWwindow *window;

    window = glfwCreateWindow(840, 840, "Laba 3", NULL, NULL);
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
