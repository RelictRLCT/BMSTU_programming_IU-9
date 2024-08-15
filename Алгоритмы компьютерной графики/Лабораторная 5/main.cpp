#include <iostream>
#include <vector>
#include "GL/freeglut.h"
#include "GL/gl.h"
#include "GLFW/glfw3.h"
#include "cmath"
using namespace std;
int window_height = 840;
int window_width = 840;
bool ctrl_pressed = false;

typedef struct _point_t {
    float x;
    float y;
    int end_code[4];
} point_t;

typedef struct _otr_t {
    point_t point1;
    point_t point2;
} otr_t;

bool need_update = true;

vector<point_t> points(0);
vector<point_t> new_points(0);
point_t points_of_otsekatel[4] = {point_t{-0.5, -0.5},
                                 point_t{0.5, -0.5},
                                 point_t{0.5, 0.5},
                                 point_t{-0.5, 0.5}};

void view_otsekatel() {
    glBegin(GL_LINES);
    glColor3f(144, 255, 113);
    glVertex2f(points_of_otsekatel[0].x, points_of_otsekatel[0].y);
    glVertex2f(points_of_otsekatel[1].x, points_of_otsekatel[1].y);

    glVertex2f(points_of_otsekatel[1].x, points_of_otsekatel[1].y);
    glVertex2f(points_of_otsekatel[2].x, points_of_otsekatel[2].y);

    glVertex2f(points_of_otsekatel[2].x, points_of_otsekatel[2].y);
    glVertex2f(points_of_otsekatel[3].x, points_of_otsekatel[3].y);

    glVertex2f(points_of_otsekatel[3].x, points_of_otsekatel[3].y);
    glVertex2f(points_of_otsekatel[0].x, points_of_otsekatel[0].y);
    glEnd();
}

void view_otrs(vector<point_t> points) {
    int points_size = points.size();
    if (points_size % 2 == 0) {
        for (int i = 0; i < points_size; i+=2) {
            glBegin(GL_LINES);
            glColor3f(0, 255, 113);
            glVertex2f(points[i].x, points[i].y);
            glVertex2f(points[i+1].x, points[i+1].y);
            glEnd();
        }
    } else {
        for (int i = 0; i < points_size - 1; i+=2) {
            glBegin(GL_LINES);
            glColor3f(0, 255, 113);
            glVertex2f(points[i].x, points[i].y);
            glVertex2f(points[i+1].x, points[i+1].y);
            glEnd();
        }
        glBegin(GL_POINTS);
        glColor3f(0, 255, 113);
        glVertex2f(points[points.size() - 1].x, points[points.size() - 1].y);
        glEnd();
    }
}

float get_otr_len(otr_t otr) {
    return sqrt(pow((otr.point2.x - otr.point1.x)*window_width/2.0, 2)
                + pow((otr.point2.y - otr.point1.y)*window_height/2.0, 2));
}

otr_t union_otrs(otr_t otr1, otr_t otr2) {
    if (otr1.point2.x == otr2.point1.x and otr1.point2.y == otr2.point1.y) {
        return otr_t{otr1.point1, otr2.point2};
    } else if (otr1.point1.x != -2) {
        return otr1;
    } else {
        return otr2;
    }
}

float epsilon = 1.5;

otr_t find_new_otr(otr_t otr) {

    if (otr.point1.end_code[0] == 0 and otr.point1.end_code[1] == 0 and  otr.point1.end_code[2] == 0 and
    otr.point1.end_code[3] == 0 and otr.point2.end_code[0] == 0 and otr.point2.end_code[1] == 0 and
    otr.point2.end_code[2] == 0 and otr.point2.end_code[3] == 0) {
        return otr;
    }

    if ((otr.point1.end_code[0] == 1 and otr.point2.end_code[0] == 1) or
        (otr.point1.end_code[1] == 1 and otr.point2.end_code[1] == 1) or
        (otr.point1.end_code[2] == 1 and otr.point2.end_code[2] == 1) or
        (otr.point1.end_code[3] == 1 and otr.point2.end_code[3] == 1)) {
        return otr_t{point_t{-2, -2}, point_t{-2, -2}};
    }

    if (get_otr_len(otr) <= epsilon) {
        return otr;
    }

    point_t seredina = {(otr.point2.x + otr.point1.x)/2, (otr.point2.y + otr.point1.y)/2};

    if (seredina.x < points_of_otsekatel[0].x) {
        seredina.end_code[3] = 1;
    } else {
        seredina.end_code[3] = 0;
    }

    if (seredina.y > points_of_otsekatel[3].y) {
        seredina.end_code[2] = 1;
    } else {
        seredina.end_code[2] = 0;
    }

    if (seredina.x > points_of_otsekatel[1].x) {
        seredina.end_code[1] = 1;
    } else {
        seredina.end_code[1] = 0;
    }

    if (seredina.y < points_of_otsekatel[0].y) {
        seredina.end_code[0] = 1;
    } else {
        seredina.end_code[0] = 0;
    }

    otr_t otr1 = otr_t{otr.point1, seredina};
    otr_t otr2 = otr_t{seredina, otr.point2};
    return union_otrs(find_new_otr(otr1), find_new_otr(otr2));
}

void form_new_points() {
    if (need_update) {
        int points_size = points.size() % 2 == 0 ? points.size() : points.size() - 1;

        for (int i = 0; i < points_size; i++) {

            if (points[i].x < points_of_otsekatel[0].x) {
                points[i].end_code[3] = 1;
            } else {
                points[i].end_code[3] = 0;
            }

            if (points[i].y > points_of_otsekatel[3].y) {
                points[i].end_code[2] = 1;
            } else {
                points[i].end_code[2] = 0;
            }

            if (points[i].x > points_of_otsekatel[1].x) {
                points[i].end_code[1] = 1;
            } else {
                points[i].end_code[1] = 0;
            }

            if (points[i].y < points_of_otsekatel[0].y) {
                points[i].end_code[0] = 1;
            } else {
                points[i].end_code[0] = 0;
            }

        }

        new_points.clear();

        for (int i = 0; i < points_size; i+=2) {
            otr_t otr = {point_t{points[i]},
                         point_t{points[i+1]}};


            otr_t new_otr = find_new_otr(otr);
            if (new_otr.point1.x != -2) {
                new_points.push_back(new_otr.point1);
                new_points.push_back(new_otr.point2);
            }
        }

        need_update = false;
    }
}

void display(GLFWwindow *window) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glClearColor(0.1, 0.2, 0.4, 0.0);
    glEnable(GL_DEPTH_TEST);
    glLoadIdentity();

    view_otsekatel();

    if (ctrl_pressed) {
        form_new_points();
        view_otrs(new_points);
    } else {
        view_otrs(points);
    }

    glFlush();
    glfwSwapBuffers(window);
    glfwPollEvents();
}


void CallbackKeys(GLFWwindow* window, int key, int scancode, int action, int mods) {

    if (action == GLFW_PRESS) {
        switch (key) {
            case GLFW_KEY_LEFT_CONTROL:
                ctrl_pressed = !ctrl_pressed;
                break;
            case GLFW_KEY_LEFT_ALT:
                points.clear();
                new_points.clear();
                break;
            case GLFW_KEY_BACKSPACE:
                if (points.size() > 0) {
                    points.pop_back();
                }
                if (new_points.size() > 0) {
                    new_points.pop_back();
                }
                break;
        }
    }
}

void mouse(GLFWwindow* window, int button, int action, int mods) {

    if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
        double xpos, ypos;
        glfwGetCursorPos(window, &xpos, &ypos);
        ypos = window_height - ypos;
        xpos -= window_width/2.0;
        ypos -= window_height/2.0;
        point_t point = {float(xpos/(window_width/2.0)), float(ypos/(window_height/2.0))};
        points.push_back(point);
        need_update = true;
    }

}

int main() {

    glfwInit();
    GLFWwindow *window;

    window = glfwCreateWindow(window_width, window_height, "Laba 5", NULL, NULL);
    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, CallbackKeys);
    glfwSetMouseButtonCallback(window, mouse);
    while (not glfwWindowShouldClose(window)) {
        display(window);
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
